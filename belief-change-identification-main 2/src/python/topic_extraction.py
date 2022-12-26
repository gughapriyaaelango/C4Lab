import pandas as pd
import json, glob, re, os, time
#Base and Cleaning 

import numpy as np
import emoji
import regex
import re
import string
from collections import Counter

#Natural Language Processing (NLP)
import spacy
import gensim
from spacy.tokenizer import Tokenizer
import gensim.parsing.preprocessing as pp
from gensim.corpora import Dictionary
from gensim.models.ldamulticore import LdaMulticore
from gensim.models.coherencemodel import CoherenceModel
from gensim.parsing.preprocessing import STOPWORDS as SW
from gensim.test.utils import datapath
from sklearn.decomposition import LatentDirichletAllocation, TruncatedSVD
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.model_selection import GridSearchCV
from pprint import pprint
from wordcloud import STOPWORDS
from kneed import KneeLocator
    

def parse_json_file(infile):
    '''
    Parses json file of historical tweets into a csv
    Accepts:
        Filename (infile)
        Output directory (output_dir)
    Returns:
        Pandas DataFrame
    '''
     # Just get the original name (no extension)
    name = infile.split("/")[-1].split(".json")[0]
    
    with open(infile,"r") as f:
        data=json.load(f)
        print(data.keys())
        df = pd.read_json(json.dumps(data['data']))
        colnames = df.columns.to_list()
        colnames.remove("tweet_created_at")
        colnames.insert(0,"tweet_created_at")
        df = df.reindex(columns = colnames)
        df = df.sort_values("tweet_created_at").reset_index().drop(["index"],axis=1)
        return df, name

def emoji_free_text(text):
    """
    Cleans  emoji's from text
    Accepts:
        Text (tweets)
    Returns:
        Text (emoji free tweets)
    """
    emoji_list = [c for c in text if c in emoji.UNICODE_EMOJI]
    clean_text = ' '.join([str for str in text.split() if not any(i in str for i in emoji_list)])
    return clean_text


def punctuation_free_text(text):
    '''
    Cleans punctuation from text
    '''

    text = " ".join(pp.preprocess_string(text,[pp.strip_numeric,pp.strip_punctuation,pp.strip_short]))
    return text

def url_free_text(text):
    '''
    Cleans text from urls
    '''
    text = re.sub(r'http\S+', '', text)
    return text

def mention_free_text(text):
    '''
    Cleans text from urls
    '''
    text = re.sub(r'@\S+', '', text)
    return text


def clean_text(df):
    '''
    Preprocess text to remove emojis, urls, mentions, and punctuation
    '''
    return(df['tweet_text'].apply(emoji_free_text).apply(url_free_text).apply(mention_free_text).apply(punctuation_free_text))

def tokenize(cleaned_text):
    '''
    Tokenize and drop stop words
    '''
    # Custom stopwords
    custom_stopwords = ['&mldr;','...','etc','don','hi','\n','\n\n', '&amp;', ' ', '.', '-', 'got', "it's", 'it’s', "i'm", 'i’m', 'im', 'want', 'like', '$', '@']
    nlp = spacy.load('en_core_web_sm')
    # Customize stop words by adding to the default list
    final_stop_words = nlp.Defaults.stop_words.union(STOPWORDS).union(custom_stopwords)
    tokens = []

    for doc in nlp.pipe(cleaned_text, batch_size=500, disable=["parser", "ner"]):
        doc_tokens = []    
        for token in doc: 
            if token.text.lower() not in final_stop_words:
                doc_tokens.append(token.lemma_.lower())   
        tokens.append(doc_tokens)

    # Makes tokens column
    return tokens

def compute_coherence_values(dictionary, corpus, texts, limit, start=2, step=3):
    """
    Compute c_v coherence for various number of topics

    Parameters:
    ----------
    dictionary : Gensim dictionary
    corpus : Gensim corpus
    texts : List of input texts
    limit : Max num of topics

    Returns:
    -------
    model_list : List of LDA topic models
    coherence_values : Coherence values corresponding to the 
    LDA model with respective number of topics
    """
    coherence_values_topic = []
    model_list_topic = []
    for num_topics in range(start, limit, step):
        model = LdaMulticore(corpus=corpus, num_topics=num_topics, id2word=dictionary)
        model_list_topic.append(model)
        coherencemodel = CoherenceModel(model=model, texts=texts, dictionary=dictionary, coherence='c_v')
        coherence_values_topic.append(coherencemodel.get_coherence())

    return model_list_topic, coherence_values_topic    
   


def run_lda(tokens,start=10,limit=60,step=3):
    # Create a id2word dictionary
    id2word = Dictionary(tokens)
    id2word.filter_extremes(no_below=2, no_above=.9)
    corpus = [id2word.doc2bow(d) for d in tokens]
    m,c = compute_coherence_values(dictionary=id2word,corpus=corpus,texts = tokens,start=start,limit=limit,step=step)
    num_topics = [x.num_topics for x in m]
    kneedle = KneeLocator(num_topics,c,curve="concave",direction="increasing")
    best = num_topics.index(kneedle.knee)
    return m[best],c[best],corpus

def weighted_jaccard(s,t):
    n = sum([x for x in map(min,zip(s,t))])
    d = sum([x for x in map(max,zip(s,t))])
    return(0 if d==0 else n/d)

def generate_topic_summary(output_fname, model):
    words = [re.findall(r'"([^"]*)"',t[1]) for t in best_model.print_topics()]
    topics = [','.join(t[0:10]) for t in words]
    with open(output_fname, "w") as f:
        for id, t in enumerate(topics): 
            f.write(f"{id},t")

def pipeline(jsonfile,output_dir, save_model = False):
    try:
        os.makedirs(output_dir,exist_ok=True)
    except:
        pass
        #Ignore the error
    tweet_data_frame, filestub = parse_json_file(jsonfile)
    
    just_text = tweet_data_frame.loc[:,('tweet_id','tweet_text')]    
    print("Cleaning text...",end="")
    cleaned_text = clean_text(tweet_data_frame)
    print("Done")
    print("Tokenizing text...",end="")
    just_text['tokens'] = tokenize(cleaned_text)
    #return just_text
    print("Done")
    print("Run LDA...",end="")
    if save_model:
        model, coherence, corpus = run_lda(just_text['tokens'])
        output = "./{}/{}.lda".format(output_dir,filestub)
        save_model(output,model)

    print("Done")
    print("Make topic assignments...",end="")
    assignments = pd.DataFrame([ dict([(f"topic_{x[0]}",x[1]) for x in y]) for y in model.get_document_topics(corpus)]).fillna(0)
    print("Done")
    #return assignments
    full_frame = pd.merge(tweet_data_frame,assignments,left_index=True,right_index=True)
    #analysis_frame = calculate_jaccards(full_frame,assignments.columns.to_list())
    output = "./{}/{}_annotated_tweets.csv".format(output_dir,filestub)
    full_frame.to_csv(output)
    
    return model, full_frame, analysis_frame
   

def save_model(output_fname,model):
   #temp_file = datapath(output_fname)
   model.save(output_fname)
   

def get_topic_labels(model):
    words = [re.findall(r'"([^"]*)"',t[1]) for t in best_model.print_topics()]
    topics = [' '.join(t[0:10]) for t in words]
    for id, t in enumerate(topics): 
        print(f"------ Topic {id} ------")
        print(t, end="\n\n")


def calculate_jaccards(tweet_df,topic_cols,win_size = 3):
    tweet_df["date_floor"] = tweet_df['tweet_created_at'].dt.floor("D")
    
    # Calculate full day frame, in case missing some
    days = pd.date_range(tweet_df["date_floor"].min(),tweet_df["date_floor"].max())
    days = pd.DataFrame({"day":days.to_list()})
    expanded_frame = pd.merge(days,tweet_df,how="left",left_on="day",right_on="date_floor")
    
    # Date and summarise topic mixtures
    tmp = expanded_frame[['day']+topic_cols].fillna(0)
    daily = tmp.groupby('day').mean()


    tmp = daily.rolling(win_size).mean().iloc[win_size-1:,:]
    tmp["id"] = tmp.index
    tmp = pd.wide_to_long(tmp,stubnames="topic_",i="id",j="topicid")
    tmp["next_weight"] = tmp.groupby(level="topicid")['topic_'].shift(-1)
    tmp = tmp.dropna().reset_index()
    j = tmp.groupby("id").apply(lambda x: weighted_jaccard(x.topic_,x.next_weight))
    j.name = "jaccard"
    
    #return frame with dates, topic mixtures, and weighted jaccard
    return pd.merge(daily,j,left_index = True, right_index = True, how="left")
    








import os
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier

import pandas as pd
import numpy as np
import nltk
import time

from selenium import webdriver 
from selenium.webdriver import ChromeOptions

from selenium.webdriver.common.by import By 
from selenium.webdriver.support.ui import WebDriverWait 
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException


def get_random():
    arr = np.random.normal(2.0,0.5,1)
    result = arr[0]
    if (result < 0): 
        result = 0.1
    return result

def main():
    test = pd.read_csv(os.path.join(os.path.dirname(__file__),'..', 'youtube-channels.csv'),
    header=0,delimiter=',', quoting=2)
    print('List of channels has %d entries.' % (len(test)))
    
    # Links to individual videos.    
    v_link_lists = list()
    channels = dict()
    
    
    driver = webdriver.Chrome()
    for i in range(0,len(test)):
        user = test.at[i,'Name']
        if not user in channels:
            channels[user] = i
        channel_url = test.at[i,'Channel']
        print('Channel for user "%s": %s' % (user, channel_url))
            
        driver.get(channel_url)
        user_data = driver.find_elements_by_xpath('//*[@id="video-title"]')
        print('  In the channel links found: %d' % len(user_data))
        count = 0
        current_list = list()
        for link_item in user_data:
            href = link_item.get_attribute('href')
            current_list.append((href,user,count))
            count += 1
        v_link_lists.append(current_list)
        time.sleep(get_random()) 
    
    for i in range(len(v_link_lists)):
        print('List of links %d' % i)
        for j in v_link_lists[i]:
            print('  %s,%s,%d' % (j[0], j[1], j[2]))
        
    
    # Now start visiting the video URLs
    time.sleep(3)    
    driver.fullscreen_window()
    
    # Timeout, if no comment content is loaded during the time.
    wait = WebDriverWait(driver, 20)
    
    for i in range(len(v_link_lists)):
        df = pd.DataFrame(columns = ['channel', 'link', 'type', 'content'])
        for video in v_link_lists[i]:
            video_url = video[0]
            channel_id = video[1]
            npk = video[2]
            
            ## Skip all videos with large numbers
            if npk > 6:
                continue        
            print('  Visiting %s' % video_url)  
            driver.get(video_url)
            
            try:
                v_id = video_url.strip('https://www.youtube.com/watch?v=')
                v_title = wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"h1.title yt-formatted-string"))).text
            except TimeoutException:
                df.loc[len(df)] = [channel_id, v_id, 'prop_log', 'TimeoutException on title']
            try:
                v_description =  wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"div#description yt-formatted-string"))).text
                df.loc[len(df)] = [channel_id, v_id, 'prop_title', v_title]
                df.loc[len(df)] = [channel_id, v_id, 'prop_description', v_description]
            except TimeoutException:
                df.loc[len(df)] = [channel_id, v_id, 'prop_log', 'TimeoutException on description']
            
            try:
                wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,'div#content yt-formatted-string#content-text'))).text
                comments =  driver.find_elements_by_css_selector('div#content yt-formatted-string#content-text')
                for comment in comments: 
                    v_comment = comment.text
                    df.loc[len(df)] = [channel_id, v_id, 'prop_comment', v_comment]
                
                time.sleep(get_random())
            except TimeoutException:
                df.loc[len(df)] = [channel_id, v_id, 'prop_log', 'TimeoutException on comments']
        
        # TODO: CHANGE THIS TO YOUR FILE PATH 
        df.to_csv (r'/home/kalvis/workspace-osx/ddgatve-stat/youtube-data/out_channel%02d.csv' % i, index = None, header=True)
        print('Saved CSV file for channel%02d' % i)

    print (df)
    driver.quit()

if __name__ == '__main__':
    main()
    

################################################
## MORE ROBUST CODE 
################################################
## https://stackoverflow.com/questions/18953499/youtube-api-to-fetch-all-videos-on-a-channel
################################################

# More Sentiment analysis
# https://www.youtube.com/watch?v=AJVP96tAWxw
    
    
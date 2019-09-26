'''
Created on Sep 18, 2019

@author: kalvis
'''


#from selenium import webdriver
#from selenium.webdriver.common.keys import Keys

from selenium import webdriver 
import pandas as pd 
from selenium.webdriver.common.by import By 
from selenium.webdriver.support.ui import WebDriverWait 
from selenium.webdriver.support import expected_conditions as EC


def main():
    driver = webdriver.Chrome() 
    driver.get('https://www.youtube.com/results?search_query=julia+lezhneva')
    user_data = driver.find_elements_by_xpath('//*[@id="video-title"]')
    links = []
    for i in user_data:
        links.append(i.get_attribute('href'))
    print(len(links))
    df = pd.DataFrame(columns = ['link', 'title', 'description', 'category'])

    wait = WebDriverWait(driver, 10)
    v_category = "CATEGORY_NAME"
    for x in links:
        driver.get(x)
        v_id = x.strip('https://www.youtube.com/watch?v=')
        v_title = wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"h1.title yt-formatted-string"))).text
        v_description =  wait.until(EC.presence_of_element_located((By.CSS_SELECTOR,"div#description yt-formatted-string"))).text
        df.loc[len(df)] = [v_id, v_title, v_description, v_category]

if __name__ == '__main__': 
    main()
    


    
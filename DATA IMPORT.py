#import untappd
import time
import urllib2
import json
import re
import os
import sys

########################################
##### Deprecated Classes/Functions #####
########################################

##class FileOperations:
##    def open_file(self, filename):
##        self.file = open(filename, 'a')
##        #self.file.write('\t',join(["checkinid","dow","day","month","year","time","venuecategory","venuelat","venuelng","beerid","beername","rating","beerstyle","abv","breweryid")
##    def close_file(self):
##        self.file.close()
##    def write_file(self,writetext):
##        self.file.write(writetext+'\n')



##def obswriter(obsfile,cid,dates,venuecat,lat,lng,bid,beername,beerstyle,abv,brewid,rating)
##    global bfile
##    bfile+=1
##    for i in dates                               
##    obsfile.write_file('\t'.join([cid,dates(0),dates(1),dates(2),dates(3),dates(4),venuecat,lat,lng,bid,beername,beerstyle,abv,brewid, rating]))

#def brewerywriter(brewfile,brewid,brew,brewlat,brewlong):
    #brewfile.write('\t'.join([brewid,brew,brewlat,brewlong])


########################################
####### Active Classes/Functions #######
########################################

def pastcheckins(f):
    with open(r'C:/constantpull/beerdata/datafile%s.txt'%(f),'r') as pastfile:
        pastfile.next()
        oldcheckins={}
        for i in pastfile:
            oldcheckins[i[0]]=1
    return oldcheckins
            
def folderwalk():
    filelist=os.listdir('c:/constantpull/beerdata')
    return len(filelist)
    
def dparse(datestring):
    redate=re.search(r'([A-Z][a-z]{2,3}),\s(\d+)\s([A-Za-z]{3,4})\s(\d{4})?\s(\d{2}:\d{2}:\d{2})', datestring, re.U)
    #return redate.groups()[0], redate.groups()[1], redate.groups()[2], redate.groups()[3], redate.groups()[4]
    return redate.groups()
                   
def jsongrab(js,beerfile,brewfile,brewdict):
    checkin=js['checkin_id']
    datetime=js['created_at']
    venue=js['venue']['primary_category']
    clat=js['venue']['location']['lat']
    clng=js['venue']['location']['lng']
    beerid=js['beer']['bid']
    beername=js['beer']['beer_name']
    beerstyle=js['beer']['beer_style']
    abv=js['beer']['beer_abv']
    breweryid=js['brewery']['brewery_id']
    brewery=js['brewery']['brewery_name']
    blat=js['brewery']['location']['lat']
    blng=js['brewery']['location']['lng']
    rating=js['rating_score']
    dow,day,month,year,time=dparse(datetime)
    beerfile.write('\t'.join([checkin,dow,day,month,year,time,venue,clat,clng,rating,beerstyle,beername,beerid,breweryid]))
    if breweryid not in brewerydict:
        brewdict[breweryid]=1
        brewfile.write('\t'.join([breweryid,brewery,blat,blng]))
    return brewdict
                       
def pullpage(apipage):
    local_json=json.load(urllib2.urlopen(apipage))
    return local_json['response']['checkins']['items']


########################################
########## Token Code For API ##########
########################################
c_id='02DB2F9695A749F8FE16FA22EB7C8087F4991805'
c_sec='DB990B7C71B44A5B39AF400683509FE6222D454B'
localhost='127.0.0.1'
tcode="EA67EDD54FED78CE769C40125ADD1EA17E0F93DA"
#client = untappd.Untappd(client_id=c_id, client_secret=c_sec, redirect_url=localhost)
urlbase='https://api.untappd.com/v4/'
urltoken='?client_id=02DB2F9695A749F8FE16FA22EB7C8087F4991805&client_secret=DB990B7C71B44A5B39AF400683509FE6222D454B'
urlpub=urlbase+"thepub/"
pullingurl=urlpub+urltoken+'&limit=25'


########################################
###### Initial Setup Code For Run ######
########################################
obsnumber=0
errorcount=0
brewerydict={}
fileno=folderwalk()
if fileno==0:
    checkindict={}
else:
    checkindict=pastcheckins(fileno-1)

########################################
#### Load/Create Brewery Dictionary ####
########################################
if os.path.isfile(r'C:/contantpull/brewerydata.txt'):
    s=open(r'C:/contantpull/brewerydata.txt')
    for b in s:
        brewerydict[0]=1
    s.close()


########################################
#### Main Loops w/Exception Handler ####
########################################    
with open('C:/constantpull/brewerydata.txt', 'a') as breweryfile:
    #while True:
    for pullno in xrange(0,5):
        try:               
            pulledobs=pullpage(pullingurl)
            starttime=time.clock()
            if obsnumber>=50000:
                checkindict=pastcheckins(fileno)
                fileno+=1
                obsnumber=0
                s=open('C:/constantpull/beerdata/datafile%s'%fileno, 'w')
                s.write('\t'.join(["checkinid","dow","day","month","year","time","venue_type","lat","long","rating","beerstyle","beername","beerid","breweryid"]))
                s.close()               
            with open('C:/constantpull/beerdata/datafile%s'%fileno, 'a') as savefile:
                    for i in pulledobs:                           
                        if len(i['venue'])>0:
                            if i['venue']['location']['venue_country']=='United States':
                                if checkin not in checkindict:
                                    checkindict[checkin]=1
                                    brewerydict=jsongrab(i,savefile,breweryfile,brewerydict)
                                    obsnumber+=1
            processtime=time.clock()-starttime
            if processtime>5:
                print "Warning - Process Time Over Five Seconds"
                print "processtime: ", processtime
            sleeptime=36-processtime
            time.sleep(sleeptime)
            errorcount=0
        except (URLerror,HTTPerror) as e:
            errorcount+=1
            print "###################################"
            print "###### NETWORK PROGRAM ERROR ######"
            print "###################################"
            if errorcount<=5:
                print "<5 Error Count Delay - Extending Cycles"
                print("\nRetrying Connection in %s cycle\n"%errorcount)
                sleeptime=errorcount*36
                time.sleep(sleeptime)
            elif errorcount<=100:
                print "Five Minute Error Count"
                totalerrortime=(errorcount-5)*5 + 9
                print("Program has %s consecutive errors, for %s minutes")
                time.sleep(300)
        except:
            errorcount+=1
            print "###################################"
            print "#### NON-NETWORK PROGRAM ERROR ####"
            print "###################################"
            e = sys.exc_info()[0]
            write_to_page( "<p>Error: %s</p>" % e )
            if errorcount<=5:
                print("Retrying in one minute")
                time.sleep(60)
            else:
                      print "Non-Network Fail"
                      print "5 Consecutive Errors Reached"
                      print "Terminating Program"
                      raise
    


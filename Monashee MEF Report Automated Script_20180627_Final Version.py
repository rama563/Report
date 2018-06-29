# -*- coding: utf-8 -*-
"""
Created on Tue Jun 19 16:52:07 2018

@author: RamaKrishna
"""

import imaplib
import os, os.path
import win32com.client
import subprocess
import email
import os
import pyodbc 
import pandas as pd
import glob
import shutil
import datetime
from datetime import timedelta
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.base import MIMEBase
from email.mime.text import MIMEText
from email import encoders

#SQL Connection
cnxn = pyodbc.connect("Driver={SQL Server Native Client 11.0};"
                      "Server=GHC27;"
                      "Database=MMLS;"
                      "Trusted_Connection=yes;")


dt=datetime.date.today().strftime("%d-%b-%Y")
now=datetime.date.today()
now_us=now-timedelta(1)
pdf_name1="MEF_Report_"+now_us.strftime("%m")+"-"+now_us.strftime("%d")+"-"+now_us.strftime("%Y")+"_With_MMLS.pdf"
pdf_name2="MEF_Report_"+now_us.strftime("%m")+"-"+now_us.strftime("%d")+"-"+now_us.strftime("%Y")+"_Without_MMLS.pdf"
os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/PDFs")
pdf_avl=os.listdir(os.getcwd())

if(now_us.weekday()<5):   
 if(pdf_name1 not in pdf_avl):   
  y_dt=now_us.strftime("%d_%m_%y")    
  os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup")
  outputdir="M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Output Files"
  inputdir="M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Input Files"
  #Deleteing Previous days files
  files=glob.glob("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Output Files/*")
  for f in files:
    os.remove(f)
  m = imaplib.IMAP4_SSL("imap.gmail.com")
  m.login("ghc@goldenhillsindia.com","GHChyd20186")
  m.select('"INBOX/Daily Files"')
  i=0
  ##Daily Trade Blotter and Risk Reward files
  p='(SUBJECT "Fwd: Daily Trade Blotter" SINCE "'+dt+'")'
  resp, items = m.search(None, p)
  items = items[0].split()
  if(len(items)==0): 
     p='(SUBJECT "FW: Daily Trade Blotter" SINCE "'+dt+'")'
     resp, items = m.search(None, p)
     items = items[0].split()
  for emailid in items:
    resp, data = m.fetch(emailid, "(BODY.PEEK[])")
    email_body = data[0][1]
    mail = email.message_from_bytes(email_body)
    for part in mail.walk():
        if part.get_content_maintype() != 'multipart' and part.get('Content-Disposition') is not None:
            open(outputdir + '/' + part.get_filename(), 'wb').write(part.get_payload(decode=True))
            i=i+1
            print(i)
            
   
  p='(SUBJECT "Monashee Position file" SINCE "'+dt+'")'
  resp, items = m.search(None, p)
  items = items[0].split()
  for emailid in items:
    resp, data = m.fetch(emailid, "(BODY.PEEK[])")
    email_body = data[0][1]
    mail = email.message_from_bytes(email_body)
    for part in mail.walk():
        if part.get_content_maintype() != 'multipart' and part.get('Content-Disposition') is not None:
            open(outputdir + '/' + part.get_filename(), 'wb').write(part.get_payload(decode=True))
            i=i+1
            print(i)
            
  p='(SUBJECT "Fwd: MMLS_Positions_'+y_dt+'" SINCE "'+dt+'")'
  resp, items = m.search(None, p)
  items = items[0].split()
  if(len(items)==0): 
     p='(SUBJECT "FW: MMLS_Positions_'+y_dt+'" SINCE "'+dt+'")'
     resp, items = m.search(None, p)
     items = items[0].split()
  for emailid in items:
    resp, data = m.fetch(emailid, "(BODY.PEEK[])")
    email_body = data[0][1]
    mail = email.message_from_bytes(email_body)
    for part in mail.walk():
        if part.get_content_maintype() != 'multipart' and part.get('Content-Disposition') is not None:
            open(outputdir + '/' + part.get_filename(), 'wb').write(part.get_payload(decode=True))
            i=i+1
            print(i)
            
  files_recd=os.listdir(outputdir)
  count=0
  os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Output Files")
  files_reqd_1=files_reqd_2=files_reqd_3=files_reqd_4="NA"
  for f in files_recd:  
      s=str(f)
      if s.startswith('Monashee_Positions'):
          files_reqd_1=f
          count=count+1
          
      if s.startswith('MMLS_Positions'):
          files_reqd_2=f
          fil_src=pd.read_excel(files_reqd_2)
          fil_src.to_csv('MMLS_Positions.csv',index=False)
          files_reqd_2="MMLS_Positions.csv"
          count=count+1  
          
      if s.startswith('Risk Reward Report'):
          files_reqd_3=f
          count=count+1   
            
      if s.startswith('Daily Trade Blotter'):
          files_reqd_4=f
          count=count+1 
          
  if((count==4) | ((count==3) & (files_reqd_2!="MMLS_Positions.csv"))):
      os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Output Files") 
      files_reqd=[files_reqd_1,files_reqd_2,files_reqd_3,files_reqd_4]
      for f in files_recd:
          if f not in (files_reqd):  
              os.remove(f)
      files_recd=os.listdir(outputdir)
      for f in files_recd:  
          if f==files_reqd_1:
                  os.rename(files_reqd_1,"Monashee_Positions.csv")
          if f==files_reqd_3:
                  os.rename(files_reqd_3,"Risk Reward Report.csv")
          if f==files_reqd_4:
                  os.rename(files_reqd_4,"Daily Trade Blotter.csv")
          
      files_recd=os.listdir(outputdir)
      #if( (len(files_recd)==4) | ((len(files_recd)==3) & ("MMLS_Positions.csv" not in files_recd))):
      source =outputdir
      dest1 = inputdir
      files = os.listdir(source)
      for f in files:
          shutil.move(source+"/"+f, dest1+"/"+f)
             
      #Downloading SQL data to CSVs
      MDD = pd.read_sql("select * from Clients_MDD_Monashee_Dummy", cnxn)
      Corr_Lkup=pd.read_sql("select Client_Symbol as Symbol,Strategy as [Current Strategy],Strategy_revised as [Correct Strategy], AssetType as [Current Asset Type],Assettype_Revised as [Correct Asset Type],Client_symbol_Revised as [Correct Ticker]  from [dbo].[Corrections_Lookup]",cnxn)
      V5=pd.read_sql("select * from [dbo].[Monashee_V5_Country] where Trade_Dt=(select max(Trade_DT) from [dbo].[Monashee_V5_Country])",cnxn)

      os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Input Files")
      V5.to_csv("V5.csv",index=False)
      MDD.to_csv("MDD.csv",index=False)
      os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Lookup Files")
      Corr_Lkup.to_csv("Corrections_Lookup.csv",index=False)
         
      #Running R Script
      print("Executing R Code")
      subprocess.call (["C:/Users/ramakrishna/Documents/R/R-3.4.4/bin/Rscript.exe", "--vanilla", "M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/MEF Report R Script 20180627_New_Setup_Version.R"])
      print("Done with R Code execution")
      #Running Excel MAcro
      os.chdir("M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/Excel Template") 

      if(files_reqd_2=='MMLS_Positions.csv'):         
         if os.path.exists("Template.xlsm"):
             print("Executing Excel Macro")
             xl=win32com.client.Dispatch("Excel.Application")
             wb=xl.Workbooks.Open(os.path.abspath("Template.xlsm"), ReadOnly=1)
             xl.Application.Run("Template.xlsm!Module1.ConsolidateFiles") 
             xl.Application.Quit() # Comment this out if your excel script closes
             del xl
             print("Done with excel macro execution, check the report in PDFs folder")
             subprocess.call(["C:/WINDOWS/system32/WindowsPowerShell/v1.0/powershell.exe","C:/Users/ramakrishna/Desktop/Python Scripts/Stop_Excel.ps1"])
             
             #Mailing MEF Report
             server = smtplib.SMTP('smtp.gmail.com', 587)    
             server.ehlo()
             server.starttls()
             server.login("ghc@goldenhillsindia.com", "GHChyd20186")
             part = MIMEBase('application', "octet-stream")
             file_path="M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/PDFs/"+pdf_name1
             part.set_payload(open(file_path, "rb").read())
             encoders.encode_base64(part)
             part.add_header('Content-Disposition', 'attachment; filename='+pdf_name1+'')
             msg = MIMEMultipart()
             text="Hi All, \n\n Please find attached Monashee Evening Flash file for "+now_us.strftime("%m-%d-%Y")+". \n\n Thanks & Regards,\n Abyn"
             msg.attach(MIMEText(text))
             msg.attach(part)
             to="abyn.scaria@goldenhillsindia.com,sravya.v@goldenhillsindia.com"
             #to="griffin@monasheecap.com,jeff@monasheecap.com,operations@monasheecap.com"
             cc="shradha.singhal@goldenhillsindia.com,ramakrishna.v@goldenhillsindia.com"
             #cc="shradha.singhal@goldenhillsindia.com,anirudh.vajjah@goldenhillsindia.com"    
             msg['From']="ghc@goldenhillsindia.com"
             msg['To']=to
             msg['Cc']=cc
             rcpt =cc.split(",") + to.split(",")
             msg['Subject']="MEF for "+now_us.strftime("%d-%b-%Y")
             server.sendmail("ghc@goldenhillsindia.com",rcpt, msg.as_string())
      else:
          if(pdf_name2 not in pdf_avl):
             if os.path.exists("Template.xlsm"):
                print("Executing Excel Macro")
                xl=win32com.client.Dispatch("Excel.Application")
                wb=xl.Workbooks.Open(os.path.abspath("Template.xlsm"), ReadOnly=1)
                xl.Application.Run("Template.xlsm!Module2.ConsolidateFiles") 
                xl.Application.Quit() # Comment this out if your excel script closes
                del xl
                print("Done with excel macro execution, check the report in PDFs folder")
                subprocess.call(["C:/WINDOWS/system32/WindowsPowerShell/v1.0/powershell.exe","C:/Users/ramakrishna/Desktop/Python Scripts/Stop_Excel.ps1"])                      
                #Mailing MEF Report
                server = smtplib.SMTP('smtp.gmail.com', 587)    
                server.ehlo()
                server.starttls()
                server.login("ghc@goldenhillsindia.com", "GHChyd20186")
                part = MIMEBase('application', "octet-stream")
                file_path="M:/Monashee Projects/Daily/MEF/MPA, MCP, Kiski/R Code/New Setup/PDFs/"+pdf_name2
                part.set_payload(open(file_path, "rb").read())
                encoders.encode_base64(part)
                part.add_header('Content-Disposition', 'attachment; filename='+pdf_name2+'')
                msg = MIMEMultipart()
                text="Hi All, \n\n Please find attached Monashee Evening Flash file (Without MMLS Positions) for "+now_us.strftime("%m-%d-%Y")+". \n\n Thanks & Regards,\n Abyn"
                msg.attach(MIMEText(text))
                msg.attach(part)
                to="abyn.scaria@goldenhillsindia.com,sravya.v@goldenhillsindia.com"
                #to="griffin@monasheecap.com,jeff@monasheecap.com,operations@monasheecap.com"
                cc="shradha.singhal@goldenhillsindia.com,ramakrishna.v@goldenhillsindia.com"
                #cc="shradha.singhal@goldenhillsindia.com,anirudh.vajjah@goldenhillsindia.com"    
                msg['From']="ghc@goldenhillsindia.com"
                msg['To']=to
                msg['Cc']=cc
                rcpt =cc.split(",") + to.split(",")
                msg['Subject']="MEF for "+now_us.strftime("%d-%b-%Y")+"_Without_MMLS"
                server.sendmail("ghc@goldenhillsindia.com",rcpt, msg.as_string())
    
  else:
      print("One or more input files are not available.")
 else:
    print("Report is already processed")
else:
    print("It's holiday, No need to run the process")
         



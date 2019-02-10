from scipy import stats
from datetime import datetime

f = open('MSFT.csv', "r")
i = 0
x = [0,0]
y =[0,0]
prev_datetime_object = None
f2 = open('output2.csv', "w") 
for line in f:
  data = line.split(',')
  datetime_object = datetime.strptime(data[0], '%m/%d/%y')
  if prev_datetime_object is not None: 
    if prev_datetime_object.month == datetime_object.month:
      x.append(datetime_object.day)
      y.append(float(data[4]))
    else:
      print "Month changed from "+ str(prev_datetime_object.month) + " to  "+ str(datetime_object.month)
      print x
      print y
      gradient, intercept, r_value, p_value, std_err = stats.linregress(x,y)
      print "Gradient and intercept", gradient, intercept
      if gradient > 0:
        print "TREND IS INCREASING FOR MONTH "+ str(prev_datetime_object.month)
        print prev_datetime_object.isoformat('-')[:-12]
        ans = str(prev_datetime_object.isoformat('-')[:-12])+ ", " + str(gradient) +  ", 1"
        f2.write(ans + "\n") 
      else:
        print "TREND IS DECREASING FOR MONTH "+ str(prev_datetime_object.month)
        print prev_datetime_object.isoformat('-')[:-12]
        ans = str(prev_datetime_object.isoformat('-')[:-12])+ ", " + str(gradient) +  ", 0"
        f2.write(ans+ "\n")      
      print "\n \n"
      x =[]
      x.append(datetime_object.day)
      y =[]
      y.append(float(data[4]))
  prev_datetime_object = datetime_object  
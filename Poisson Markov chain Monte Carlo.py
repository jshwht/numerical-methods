import numpy
import math
import random
import statistics

data = (10,8,6,7,5,6,5,7,4,3,5,2,5,11,6,7,8,9,3,4,5,6)

mu = sum(data)/len(data)
print("Analytical mu is",mu)

def numerator(theta):
    prod = 1
    for x in range(len(data)):
        term = (theta**(data[x]))*(math.exp(-theta))/(math.factorial(data[x]))
        prod = prod*term

    if theta>0 and theta<=10:
        num = prod*(0.1)
    else:
        num = 0
    return(num)

def denominator(theta_0):
    prod = 1
    for x in range(len(data)):
        term = (theta_0**(data[x]))*(math.exp(-theta_0))/(math.factorial(data[x]))
        prod = prod*term

    if theta_0 >= 0 and theta_0 <= 10:
        den = prod*(0.1) ## (prior ~ DiscU[1,10] --> 1/10)
    else:
        den = 0
    return(den)

def generate_new_theta(theta_0):
    theta = numpy.random.normal(loc=theta_0,scale=1) ## Normal proposal distribution
    r = numerator(theta)/denominator(theta_0)
    if r>random.randint(0,1):
        return(theta)
    else:
        return(0)

def generate_samples():

    samplelist = []
    theta_0 = 3

    for i in range(1000000):
        b=0
        while b==0:
            theta = generate_new_theta(theta_0)

            if theta != 0:
                b=1

        samplelist.append(theta)

        theta_0=theta

    return(samplelist)

print("MCMC mu is",statistics.mean(generate_samples()[50000:1000000]))

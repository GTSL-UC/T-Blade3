#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Aug 17 16:35:36 2021

@author: hamw
"""

def RE_calc(mu,L,v,rho):
    re = rho*v*L/mu
    
    return re


L = 0.14144E-01
mu = 20.791E-6
v = 192.1
rho = 52.98

print(RE_calc(mu,L,v,rho))

L = 0.88862E-02
mu = 20.791E-6
v = 142.9
rho = 56.56

print(RE_calc(mu,L,v,rho))
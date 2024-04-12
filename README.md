# Limitations dans la vie quotidienne
19/01/2023

## Description

Ce projet utilise les données SRCV de l'Insee pour calculer la prévalence des limitations dans la vie quotidienne liées à la santé, par âge, sexe et catégorie socio-professionnelle en 2016-2018, et par âge, sexe et année entre 2009 et 2019.

L'intérêt de ces données croisées est de les combiner avec les tables de mortalité pour calculer une espérance de vie sans incapacité et une espérance de retraite sans incapacité par PCS et sexe en 2016-2018, et par sexe et par année entre 2009 et 2019.

Les résultats de la recherche associée sont présentés dans un article, dont la version actuelle est ici : https://hal.science/hal-04030089


## Réutilisation

Pour se servir de ce code, il faut disposer des micro-données de l'enquête SRCV, accessibles aux chercheurs sur le site du réseau Quêtelet. Il suffit ensuite de lancer les différents programmes appelés dans `00_Main.R`

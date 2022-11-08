# carpediem


## Getting started

To make it easy for you to get started with GitLab, here's a list of recommended next steps.

Already a pro? Just edit this README.md and make it your own. Want to make it easy? [Use the template at the bottom](#editing-this-readme)!

## Add your files

- [ ] [Create](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#create-a-file) or [upload](https://docs.gitlab.com/ee/user/project/repository/web_editor.html#upload-a-file) files
- [ ] [Add files using the command line](https://docs.gitlab.com/ee/gitlab-basics/add-file.html#add-a-file-using-the-command-line) or push an existing Git repository with the following command:

```
cd existing_repo
git remote add origin https://gitlab.ofb.fr/Frederic.QUEMMERAIS-AMICE/carpediem.git
git branch -M main
git push -uf origin main
```

## Integrate with your tools

- [ ] [Set up project integrations](https://gitlab.ofb.fr/Frederic.QUEMMERAIS-AMICE/carpediem/-/settings/integrations)

## Collaborate with your team

- [ ] [Invite team members and collaborators](https://docs.gitlab.com/ee/user/project/members/)
- [ ] [Create a new merge request](https://docs.gitlab.com/ee/user/project/merge_requests/creating_merge_requests.html)
- [ ] [Automatically close issues from merge requests](https://docs.gitlab.com/ee/user/project/issues/managing_issues.html#closing-issues-automatically)
- [ ] [Enable merge request approvals](https://docs.gitlab.com/ee/user/project/merge_requests/approvals/)
- [ ] [Automatically merge when pipeline succeeds](https://docs.gitlab.com/ee/user/project/merge_requests/merge_when_pipeline_succeeds.html)

## Test and Deploy

Use the built-in continuous integration in GitLab.

- [ ] [Get started with GitLab CI/CD](https://docs.gitlab.com/ee/ci/quick_start/index.html)
- [ ] [Analyze your code for known vulnerabilities with Static Application Security Testing(SAST)](https://docs.gitlab.com/ee/user/application_security/sast/)
- [ ] [Deploy to Kubernetes, Amazon EC2, or Amazon ECS using Auto Deploy](https://docs.gitlab.com/ee/topics/autodevops/requirements.html)
- [ ] [Use pull-based deployments for improved Kubernetes management](https://docs.gitlab.com/ee/user/clusters/agent/)
- [ ] [Set up protected environments](https://docs.gitlab.com/ee/ci/environments/protected_environments.html)

***

# Editing this README

When you're ready to make this README your own, just edit this file and use the handy template below (or feel free to structure it however you want - this is just a starting point!). Thank you to [makeareadme.com](https://www.makeareadme.com/) for this template.

## Suggestions for a good README
Every project is different, so consider which of these sections apply to yours. The sections used in the template are suggestions for most open source projects. Also keep in mind that while a README can be too long and detailed, too long is better than too short. If you think your README is too long, consider utilizing another form of documentation rather than cutting out information.

## Name
Choose a self-explaining name for your project.

## Description


Projet          : code développé dans le cadre du projet CARPEDIEM (2016-2018) <br/>
Date            : 2022/10/06 <br/>
Version code    : 1.0 <br/>
Version R       : R-3.5.1 <br/>
Auteurs         : Alice Vanhoutte-Brunier, Julien Barrere <br/>
Contributeurs   : Frederic Quemmerais-Amice, Guilhem Autret <br/>
Contact         : frederic.quemmerais-amice@ofb.gouv.fr <br/>
Office Français de la Biodiversité (https://www.ofb.gouv.fr/) <br/>


## Méthodologie
La méthode d'analyse et un premier exemple d'utilisation sont publié dans l'article suivant : <br/>
Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020. <br/>
A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats. Frontiers in Marine Science. 7:569205 <br/>
**https://doi.org/10.3389/fmars.2020.569205**


## Installation et utilisation
**Lire la documentation technique disponible dans le dossier "documentation" :** <br/>
Barrere Julien, Autret Guilhem, Quemmerais-Amice Frédéric, 2022. Guide d'utilisation des outils d'analyses de données du projet Carpediem: cartographie du risque d'effets cumulés sur les habitats benthiques. Version 7.1 publique, octobre 2022, 149 pages.

**Téléchargez et dézippez le dossier "CODES_CARPEDIEM.zip" contenu dans le dossier "application_carpediem" sur votre ordinateur, ne changez pas le nom du dossier créé.** <br/>
<br/>

**Déroulement général de l'analyse avec le script "analyse_etape1_refc_publication_v1.R"** <br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise l'analyse par défaut (c'est à dire cumulative) du risque d'effets cumulés entre des pressions anthropiques et des habitats benthiques. <br/>

Les paramétrages de l'analyse sont renseignés dans un fichier Excel contenu dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 66 et 69. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des résultats dans la base de données, dans de nouvelles tables, dans un schéma à spécifier. <br/>
<br/>
Les différentes étapes du script sont : <br/>
ETAPE 01 - LECTURE DE FONCTIONS DECRITES DANS DU CODE SOURCE <br/>
ETAPE 02 - LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config <br/>
ETAPE 03 - IMPORT DES TABLES DE LA BASE DE DONNEES <br/>
ETAPE 04 - GESTION DES INDICES ET DIMENSIONS, stockage dans des listes <br/>
ETAPE 05 - DONNEES HABITATS ET SENSIBILITE <br/>
ETAPE 06 - CARTOGRAPHIE DES ACTIVITES <br/>
ETAPE 07 - CARTOGRAPHIE DES PRESSIONS <br/>
ETAPE 08 - CARTOGRAPHIE DU RISQUE D'EXPOSITION PRESSION / MULTI-PRESSIONS <br/>
ETAPE 09 - CALCUL DU RISQUE D'EFFETS // RISQUE D'EFFETS CONCONMITANTS <br/>
ETAPE 10 - REALISATION ET SAUVEGARDE DE GRAPHIQUES REPRESENTATIFS DES RESULTATS <br/>
ETAPE 11 - EXPORT DES RESULTATS SOUS FORME DE TABLES DANS LA BASE DE DONNEES <br/>
ETAPE 12 - CALCUL D'UN INDICE DE CONFIANCE POUR LES RELATIONS THEORIQUES <br/>
<br/>
<br/>



**Déroulement général de l'analyse avec le script "analyse_etape2_monte_carlo_publication.R" : **<br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise l'analyse du risque d'effets cumulés entre des pressions anthropiques et des habitats benthiques en réalisant des simulations de Monte-Carlo qui permettent à chaque simulation de choisir des valeurs au hasard pour 7 critères intervenants dans le calcul. <br/>
Il est imperatif d'avoir réalisé l'analyse étape 1 (script "analyse_etape1_refc_publication_v1.R") au préalable. <br/>
Les paramétrages de l'analyse doivent être renseignés dans le fichier de paramétrage .xlsx utilisé à l'étape précédente et stocké dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Seul l'onglet "simulation_monte_carlo" est a renseigner. <br/>
Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 63 et 65. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des résultats dans la base de données, dans de nouvelles tables, dans un schéma à spécifier. <br/>
<br/>

Les différentes étapes du script sont : <br/>
ETAPE 1 - IMPORT DES DONNEES ACTIVITES ET HABITATS <br/>
ETAPE 2 - CODAGE DES FONCTIONS ASSOCIEES AUX FACTEURS <br/>
ETAPE 3 - REALISATION DES SIMULATIONS <br/>
<br/>
<br/>


**Déroulement général de l'analyse avec le script "analyse_etape3_graphiques_publication.r" :** <br/>
Lire le guide d'utilisation pour obtenir des informations plus détaillées. <br/>
Le script réalise des analyses statistiques et des graphiques à partir des resultats de l'étape 1 et sur des zones d'intérêts définis par l'utilisateur. <br/>
Il est imperatif d'avoir réalisé l'analyse étape 1 (script "analyse_etape1_refc_publication_v1.R") au préalable. <br/>
Les paramétrages de l'analyse doivent être renseignés dans le fichier de paramétrage .xlsx utilisé à l'étape précédente et stocké dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". <br/>
Seul l'onglet "statistiques_par_zones" est a renseigner. <br/>

Dans le code, seuls les paramètres **"fileparam"** et **"userdir"** correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes 64 et 66. <br/>
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante). <br/>
Export des graphiques au format image dans le dossier "OUTPUTS/graphs". <br/>
<br/>

Les différentes étapes du script sont : <br/>
ETAPE 1 - LECTURE DU FICHIER DE PARAMETRAGE ET IMPORT DES DONNEES <br/>
ETAPE 2 - PRODUCTION D'UNE MATRICE ACTIVITE PRESSION REDUITE AUX ACTIVITES ET PRESSIONS PRISES EN COMPTE DANS L'EVALUATION <br/>
ETAPE 3 - CREATION DES GRAPHES <br/>

## Support
Tell people where they can go to for help. It can be any combination of an issue tracker, a chat room, an email address, etc.

## Roadmap
If you have ideas for releases in the future, it is a good idea to list them in the README.

## Contributing
State if you are open to contributions and what your requirements are for accepting them.

For people who want to make changes to your project, it's helpful to have some documentation on how to get started. Perhaps there is a script that they should run or some environment variables that they need to set. Make these steps explicit. These instructions could also be useful to your future self.

You can also document commands to lint the code or run tests. These steps help to ensure high code quality and reduce the likelihood that the changes inadvertently break something. Having instructions for running tests is especially helpful if it requires external setup, such as starting a Selenium server for testing in a browser.

## Authors and acknowledgment
Show your appreciation to those who have contributed to the project.

## Licence
Ce programme est un logiciel libre diffusé sous les termes de la licence publique générale GNU (GNU General Public License, GNU GPL) version 3 ou toute version ultérieure. Vous pouvez consulter le guide rapide de la GNU GPL v3 sur https://www.gnu.org/licenses/quick-guide-gplv3.fr.html. Vous pouvez redistribuer et/ou modifier le contenu de ce programme suivant les termes de la GNU GPL version 3 ou ultérieure telle que publiée par la Free Software Foundation. Consultez la GNU General Public License pour plus de details.

## Responsabilité
Ce programme est diffusé dans l'espoir qu'il sera utile. Les auteurs, les contributeurs et l'OFB n'offrent aucune garantie de fonctionnement et de résultat liée à l'utilisation de ce programme. Ils ne peuvent en cas être tenu responsables des interprétations et des conclusions qui pourraient être faites suite l'utilisation de ce programme.

## Statut du projet
Aucun nouveau développement n'est prévu prochainement.

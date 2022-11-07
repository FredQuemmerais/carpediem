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


Projet          : code développé dans le cadre du projet CARPEDIEM (2016-2018)
Date            : 2022/10/06
Version code    : 1.0
Version R       : R-3.5.1
Auteurs         : Alice Vanhoutte-Brunier, Julien Barrere
Contributeurs   : Frederic Quemmerais-Amice, Guilhem Autret
Contact         : frederic.quemmerais-amice@ofb.gouv.fr

Office Français de la Biodiversité (https://www.ofb.gouv.fr/)



## Installation et utilisation
Lire la documentation technique disponible dans le dossier "documentation"
Barrere Julien, Autret Guilhem, Quemmerais-Amice Frédéric, 2022. Guide d'utilisation des outils d'analyses de données du projet Carpediem: cartographie du risque d'effets cumulés sur les habitats benthiques. Version 7.1 publique, octobre 2022, 149 pages.


Déroulement général de l'analyse du script 1 "" (voir le guide d'utilisation :                                             
Les paramétrages de l'analyse sont renseignés dans un fichier Excel contenu dans le dossier "PARAM" du dossier "CODES_CARPEDIEM". Dans le code, seuls les paramètres "fileparam" et "userdir" correspondant respectivement au nom du fichier .xlsx de paramétrage et au chemin d'accès du dossier "CODES_CARPEDIEM" sont a renseigner aux lignes x et y.
Lecture et import des tables de données sources contenues dans une base de données PostgreSQL (locale et/ou distante).
Export des résultats dans la base de données, dans de nouvelles tables, dans un schéma à spécifier.

Les différentes étapes du script sont :
ETAPE 01 - LECTURE DE FONCTIONS DECRITES DANS DU CODE SOURCE
ETAPE 02 - LECTURE DES PARAMETRES DU FICHIER EXCEL ET ECRITURE DANS LA LISTE config
ETAPE 03 - IMPORT DES TABLES DE LA BASE DE DONNEES
ETAPE 04 - GESTION DES INDICES ET DIMENSIONS, stockage dans des listes     
ETAPE 05 - DONNEES HABITATS ET SENSIBILITE   
ETAPE 06 - CARTOGRAPHIE DES ACTIVITES
ETAPE 07 - CARTOGRAPHIE DES PRESSIONS
ETAPE 08 - CARTOGRAPHIE DU RISQUE D'EXPOSITION PRESSION / MULTI-PRESSIONS
ETAPE 09 - CALCUL DU RISQUE D'EFFETS // RISQUE D'EFFETS CONCONMITANTS
ETAPE 10 - REALISATION ET SAUVEGARDE DE GRAPHIQUES REPRESENTATIFS DES RESULTATS
ETAPE 11 - EXPORT DES RESULTATS SOUS FORME DE TABLES DANS LA BASE DE DONNEES
ETAPE 12 - CALCUL D'UN INDICE DE CONFIANCE POUR LES RELATIONS THEORIQUES







Etape 2 :




Etape 3 : 


## Méthodologie
La méthode d'analyse et un premier exemple d'utilisation est publié dans l'article suivant :
Quemmerais-Amice Frédéric, Barrere Julien, La Rivière Marie, Contin Gabriel, Bailly Denis, 2020. A Methodology and Tool for Mapping the Risk of Cumulative Effects on Benthic Habitats. Frontiers in Marine Science. 7:569205. https://doi.org/10.3389/fmars.2020.569205

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

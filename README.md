## Methodes du Datascientist : Analyse et explication des facteurs explicatifs pour les défauts de paiements


Notre base est un jeu de données bancaire taiwanais qui comporte 25 variables:

• L’identifiant client

• La variable à expliquer de défaut de paiement (1=Oui, 0=Non)

• Le montant de crédit accordé

• Le genre (1=Homme, 2=Femme)

• Le niveau d’éducation

• Le statut marital

• L'âge

• 6 variables qui indique l’état du remboursement de septembre à avril

• 6 variables qui donnent le montant du relevé de la facture de septembre à avril

• 6 variables du montant du paiement le mois précédent de septembre à avril








On décide de mettre en facteurs plusieurs variables afin d’en faciliter l’interprétation et l’utilisation
notamment dans la modélisation et d’ajouter des variables qui va nous permettre d’avoir plus d’informations
dans les analyses :

La variable à expliquer de défaut de paiement = « Oui » pour défaut de paiement, sinon « Non »

La variable de genre = « Homme » pour 1, « Femme » pour 2.

La variable de statut marital = « Mariage » pour 1, « Célibataire pour 2, sinon « Autres »

La variable niveau d’éducation = « EtudesSup » pour 1, « Université » pour 2, « Lycée » pour 3, « Autres » sinon

On ajoute les variables notamment « Pay » qui est égale a « Oui » si le client n’a pas de remboursement de
avril à septembre.

De plus, on ajoute une variable qui decoupe et classe en catégories la variable de l’age

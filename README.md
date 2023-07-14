# shortCDI_sims
The project uses item response theory and Bayesian-based parameterization to select test items tailored to each child's ability in vocabulary assessment. The model consists of two main components: Bayesian-based parameterization of normative data and the utilization of item response theory. Normative data from language-, gender-, and age-matched children is obtained from Wordbank and test items are fitted with normal distributions. Estimated scores are obtained through an affine transformation, and the 2PL Model is used in the item response theory-based computerized adaptive test to estimate the ability parameter of each child.

This is a project from my PhD studies. The model involves:
1) Initial parameterization of existing vocabulary data.
2) During administration, implementation of IRT is conducted to select items that are maximally adjusted to the child's latent ability, with the aim to reduce number of test items needed. 
3) A full, estimated score is computed for each child, by utilising the test scores and the initial parameterization during (1).

The model consists of two main components: Bayesian approach to paramerization of data and the utilization of Item Response Theory.

Bayesian-based parameterization of normative data:
1. Language-, gender-, and age-matched children are retrieved from Wordbank.
2. For each test item i that is reported by the parent as either known or not known by the child j, a histogram of full CDI scores of all other children with the same response is extracted.
3. The resulting histogram is fitted with a normal distribution using maximum likelihood estimation. 
4. This procedure is repeated as many times as there are items on the word list. 
5. The histograms of all test items are then log-summed, and we retrieve the mode m of the resulting histogram. 
6. The estimated full-CDI scores, s is obtained via affine transformation of the mode m.

Item response theory-based computerised adaptive test:
2PL Model, where the following parameters are used: Discrimination parameters αi; and Difficulty parameters βi to estimate the Ability parameter of child j, θj

# Applied Data Science Master Thesis, Utrecht University


## Overview

This project replicates findings from studies with open data and assesses the quality of synthetic data generation methods (SDGMs).

## Methodology

### 1. Replication
Identify and replicate: Selected studies from open journals and replicated their findings.

### 2. Dataset Creation
Four datasets were created for evaluation:

a) Bootstrapping (Resampling with Replacement)

Purpose: Theoretical maximum reference.
Goal: Find methods performing nearly as well.
b) Bootstrapping with Independent Variables

Purpose: Simplest baseline to outperform.
Expectation: Not expected to perform well.
c) Synthetic Data Generation Models (SDGMs)

From the synthpop R package:

Parametric Model: Uses linear, logistic, and polytomous regressions.
CART (Classification and Regression Trees): Captures non-linear relationships.

### 3. Evaluation Methods

Two utility measures used:

a) Root Mean Square Error (RMSE)

Purpose: Accuracy of replication.
Interpretation: Lower values are better.
b) Confidence Interval Overlap (CIO)

Purpose: Similarity of estimates.
Interpretation: High overlap indicates similarity.


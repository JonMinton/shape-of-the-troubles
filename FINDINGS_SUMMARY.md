# OLS Exhaustive Scan: Key Findings and Manuscript Implications

## High-level results

49 HMD populations scanned with the impulse-decay model (k=0.097, ages 15-45). Two runs: (1) onset fixed at 1972, (2) onset scanned over all candidate years.

### Onset year scanning results (key countries)

| Country | Best onset | Interpretation |
|---------|-----------|----------------|
| **NI** | **1972** | Troubles — exactly right |
| **East Germany** | **1990** | Reunification shock — was incorrectly identified as 1972 |
| **West Germany** | **1970** | Post-war mortality improvement inflection |
| **Spain** | **1936** | Civil War onset — correct |
| **Finland** | **1939** | Winter War — correct |
| **Portugal** | **1953** | Colonial wars era onset |
| **USA** | **1948** | Post-WWII male mortality plateau |
| **Canada** | **1972** | Mortality transition (not conflict) |
| **Israel** | **2000** | Second Intifada era |

The onset scanner is remarkably effective at finding historically meaningful breakpoints.

### Bayesian posterior estimates (key countries, rstanarm)

| Country | Onset | Male impulse [95% CI] | P(γ>0) | Interpretation |
|---------|-------|----------------------|--------|----------------|
| **NI** | 1972 | 0.246 [0.205, 0.282] | 1.000 | Strong positive — Troubles confirmed |
| **East Germany** | 1990 | 0.163 [0.137, 0.196] | 1.000 | Strong positive — reunification mortality shock |
| **Spain** | 1936 | 0.531 [0.492, 0.569] | 1.000 | Strong positive — Civil War |
| **Finland** | 1939 | 0.675 [0.632, 0.721] | 1.000 | Strong positive — Winter War |
| **Israel** | 2000 | 0.082 [0.045, 0.123] | 1.000 | Positive — Second Intifada |
| **Canada** | 1972 | 0.064 [0.041, 0.088] | 1.000 | Positive — NOT conflict (mortality transition) |
| **West Germany** | 1970 | 0.024 [0.001, 0.049] | 0.977 | Marginal positive |
| **Scotland** | 1972 | -0.272 [-0.306, -0.238] | 0.000 | Negative — mortality *improving* after 1972 |
| **Norway** | 1972 | -0.266 [-0.300, -0.228] | 0.000 | Negative — mortality *improving* |
| **Sweden** | 1972 | -0.094 [-0.121, -0.066] | 0.000 | Negative — mortality *improving* |
| **USA** | 1948 | -0.225 [-0.247, -0.203] | 0.000 | Negative — post-WWII mortality *improvement* |
| **Portugal** | 1953 | -0.342 [-0.376, -0.309] | 0.000 | Negative — post-war mortality *improvement* |

Key insight: With onset scanning, the **direction** of the impulse becomes the primary discriminator. Countries where mortality worsened at onset (positive impulse = conflict/disruption) are clearly separated from countries where the model captures mortality improvement (negative impulse = secular trends). The sign flips that appeared as "false positives" in the fixed-1972 scan are now correctly identified as improvement trends.

## Surprising findings

### 1. Canada is the strongest detection, not Northern Ireland

Canada (AIC=759) exceeds NI (AIC=599) substantially. Peak age 18, M/F ratio 1.41. This likely reflects a genuine post-1972 male mortality divergence -- possibly drug/alcohol/injury deaths in young men ("deaths of despair" avant la lettre), or Vietnam draft dodger cohort effects. **This is NOT a conflict signature** but the model picks it up because the shape (post-1972 male excess, decaying over time) happens to match. This is an important specificity finding: the impulse-decay with fixed onset at 1972 detects any post-1972 male mortality departure, not just conflict.

**Implication:** Strengthens the case for the sex-ratio diagnostic and for scanning over onset year rather than fixing at 1972. When onset is freed, NI should strongly prefer 1972 while Canada's "onset" will likely shift or disappear.

### 2. Northern Ireland has the highest M/F ratio among strong detections

NI's M/F ratio of 2.00 is the highest among any country with AIC > 50. The next closest is Australia (1.54) and Canada (1.41). This validates the sex-ratio diagnostic: NI's signature is distinctively male-dominated, consistent with sectarian/combatant violence.

### 3. Many strong detections have M/F ratio < 1 (female-dominant)

Scotland (0.61), Norway (0.43), Sweden (0.42), Finland (0.52), Belgium (0.61), France civilian (0.55), Italy (0.60). These countries show **stronger female** impulse effects. This is almost certainly not conflict -- it likely reflects sex-differential trends in mortality improvement post-1972 (e.g., cardiovascular revolution benefiting men more, or female-specific risks). The model absorbs these secular trends into the impulse-decay component because they happen to have the right temporal shape.

**Implication:** M/F ratio < 1 is a strong signal of NOT-conflict. The paper should emphasise this as a key discriminator. A table grouping countries into M/F > 1.5 (possible conflict/male risk), 1.0-1.5 (ambiguous), < 1.0 (non-conflict trend) would be powerful.

### 4. Germany (unified) has enormous impulse coefficients

DEUTNP has peak_effect=9.07 and mean male impulse=3.75 -- an order of magnitude larger than any other country. This is an artefact: the "unified" Germany series (1990-2021) began after reunification, and the model is fitting the post-reunification mortality convergence as if it were an "impulse-decay." The quadratic baseline is insufficient for this structural break.

**Implication:** DEUTNP should be flagged or excluded as a data quality issue. East and West Germany separately are more meaningful.

### 5. Negative controls work well

Baltic states (Latvia -39, Lithuania -31), Bulgaria (-45), Russia (-38), Central/Eastern Europe generally show negative AIC improvement. The model correctly finds that adding the impulse-decay term makes things *worse* -- there's no post-1972 improvement to capture, because these countries experienced *worsening* male mortality after 1972 (alcohol, cardiovascular crises). This is the anti-pattern: where the "impulse" would need to go the wrong way.

### 6. Portugal has the highest M/F ratio (2.50) but weak detection

Portugal: AIC=2.6, M/F=2.50. The Colonial Wars (1961-74) produced a strongly male signature (conscripts in Africa), but the weak AIC suggests the overall magnitude is small in the all-cause data. This is a true positive that's below the detection threshold -- consistent with Portugal's wars being fought overseas with relatively few casualties compared to total population.

**Implication:** Portugal is an excellent discussion case. It shows the method can identify the right sex pattern even when signal is weak.

## Critical methodological issue: fixed onset year

The single biggest limitation exposed by this scan is **fixing the onset at 1972**. This was calibrated for the Troubles but:
- Canada's strong detection is an artefact of 1972 coinciding with other mortality transitions
- Many European countries show strong detections because post-1972 happens to capture the cardiovascular revolution / mortality improvement acceleration
- Countries with pre-1972 conflicts (Portugal Colonial Wars 1961-74, Greece Civil War 1946-49) would be missed

**Recommendation:** Phase 3 of the workplan (scanning over onset year) becomes critical. The paper should present the fixed-onset scan as a preliminary exercise, then show how results change when onset is freed.

## Proposed manuscript changes

### Structural changes

1. **Add Canada as a "false positive" case study** alongside NI and Germany. Show the raw counts (which will reveal it's clearly not conflict), then use it to motivate why sex ratio and onset scanning are necessary.

2. **Reframe the scan results around a 2x2 classification:**

   |                     | M/F > 1.5 (male-dominated) | M/F < 1.0 (trend/non-conflict) |
   |---------------------|---------------------------|-------------------------------|
   | Strong detection    | **NI** (conflict)         | Scotland, Norway, Finland (secular trend) |
   | Weak/no detection   | Portugal (weak conflict)  | Baltic states, Russia (deterioration) |

3. **Add a "lessons from false positives" subsection** in the discussion, explaining why Canada, Scotland, Norway etc. are detected and what the sex-ratio diagnostic tells us.

4. **Present Germany results more carefully:** Drop DEUTNP from the main scan (artefact), keep DEUTE and DEUTW as illustrative cases of reunification effects vs. genuine mortality dynamics.

### Figures to add to main text

- **Scatter plot of AIC vs M/F ratio** (already added -- good)
- **Forest plot of impulse coefficients** for top 15 countries, male and female side by side
- **Small multiples of raw death counts** for the ~10 most interesting cases (NI, Canada, Scotland, Portugal, Finland, East Germany, West Germany, USA, France, Sweden)

### Interpretation to revise

The abstract mentions Algeria (still referenced) and claims 75% capture -- both need updating. The abstract should emphasise the scan of 49 populations and the sex-ratio diagnostic as key contributions, not just NI.

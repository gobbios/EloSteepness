functions {
  array[] real ProbFunction(array[] real EloStart, real k, matrix presence, int N, int K, array[] int Ai, array[] int loser, real diff_f) {
    array[N] real result;
    real toAdd;
    //real aux_mean = 0.0;
    vector[K] EloNow;
    for (j in 1:K) {
      EloNow[j] = EloStart[j];
    }
    for (i in 1:N) {
      // centering:
      EloNow = EloNow - dot_product(row(presence, i), EloNow) / sum(row(presence, i));
      // likelihood contribution:
      result[i] = 1/(1 + exp(diff_f * (EloNow[loser[i]] - EloNow[Ai[i]])));
      // update addend:
      toAdd = (1 - result[i]) * k;
      // update:
      EloNow[Ai[i]] = EloNow[Ai[i]] + toAdd;
      EloNow[loser[i]] = EloNow[loser[i]] - toAdd;
      }
    return result;
  }

  vector cum_winprob(vector EloStart, real k, int n_interactions, int n_ids, array[] int Ai, array[] int loser) {
    real single_wp;
    real toAdd;
    matrix[n_ids, n_ids] pairwise_winprobs;
    vector[n_ids] cumwinprobs;

    vector[n_ids] EloNow;
    for (j in 1:n_ids) {
      EloNow[j] = EloStart[j];
    }
    for (i in 1:n_interactions) {
      single_wp = 1/(1 + exp(EloNow[loser[i]] - EloNow[Ai[i]]));
      // update added:
      toAdd = (1 - single_wp) * k;
      // update:
      EloNow[Ai[i]] = EloNow[Ai[i]] + toAdd;
      EloNow[loser[i]] = EloNow[loser[i]] - toAdd;
    }

    for (i in 1:(n_ids - 1)) {
      for (j in (i + 1):n_ids) {
        single_wp = 1/(1 + exp(EloNow[i] - EloNow[j]));
        pairwise_winprobs[j, i] = single_wp;
        pairwise_winprobs[i, j] = 1.0 - single_wp;
      }
    }
    for (i in 1:n_ids) {
      pairwise_winprobs[i, i] = 0.0;
      cumwinprobs[i] = sum(pairwise_winprobs[i, ]);
    }
    return cumwinprobs;
  }

  vector cumwinprob2steep(vector nds, data int n_ids) {
    // aux objects for slope
    vector[n_ids] A1;
    vector[n_ids] A2;
    vector[n_ids] B1;
    vector[n_ids] AB1;
    // aux objects for intercept
    real sum_y;
    real sum_x2;
    real sum_x;
    real sum_xy;
    // aux for ranks
    vector[n_ids] theranks;
    int r;
    int s;
    // results
    vector[2] xsteep;

    // theranks = to_vector(sort_indices_desc(nds));
    // do the ranks
    for (i in 1:n_ids) {
      r = 1;
      s = 1;
      for (j in 1:i) {
        if (nds[j] < nds[i]) {
          r = r + 1;
        }
        if (nds[j] == nds[i]) {
          s = s + 1;
        }
      }

      for (j in (i + 1) : n_ids) {
        if (nds[j] < nds[i]) {
          r = r + 1;
        }
        if (nds[j] == nds[i]) {
          s = s + 1;
        }
      }
      theranks[i] = r + (s - 1) * 0.5 - 0.5;
    }

    // the intercept
    sum_y = sum(nds);
    sum_x2 = 0.0;
    for (i in 1:n_ids) {
      sum_x2 = sum_x2 + theranks[i] ^ 2;
    }
    sum_x = sum(theranks);
    sum_xy = sum(nds .* theranks);

    xsteep[1] = ((sum_y * sum_x2) - (sum_x * sum_xy)) / (((n_ids * sum_x2) - sum_x ^ 2));

    // the slope
    A1 = theranks - mean(theranks);
    B1 = nds - mean(nds);
    AB1 = A1 .* B1;
    A2 = A1 .* A1;
    xsteep[2] = sum(AB1) / sum(A2);

    return xsteep;
  }

}

data {
  int<lower=1> N; // number of encounters
  int<lower=1> K; // number of individuals
  int<lower=1> n_rand; // number of randomized sequences
  array[N, n_rand] int<lower=1> winner; // winner's index
  array[N, n_rand] int<lower=1> loser; // losers's index
  matrix[N, K] presence;
  array[N] int<lower=0> y; // always 1
  real<lower=0> diff_f; // Elo Score difference factor
}

parameters {
  array[n_rand, K] real EloStart_raw;
  array[n_rand] real<lower=0.0> k_raw;
  array[n_rand] real<lower=0.0> sigma_raw;
}

transformed parameters {
  array[n_rand, K] real EloStart;
  array[n_rand] real<lower=0.0> k;
  for (r in 1:n_rand) {
    for (i in 1:K) {
      EloStart[r, i] = EloStart_raw[r, i] - mean(EloStart_raw[r, ]);
    }
   for (i in 1:K) {
      EloStart[r, i] = EloStart[r, i] / diff_f;
    }
   k[r] = k_raw[r]/diff_f;
  }

}

model {
  for (r in 1:n_rand) {
    k_raw[r] ~ normal(0, 1);
    sigma_raw[r] ~ normal(0, 1);
    EloStart_raw[r, ] ~ normal(0, sigma_raw[r]);
    y ~ bernoulli(ProbFunction(EloStart[r, ], k[r], presence, N, K, winner[, r], loser[, r], diff_f));
  }

}

generated quantities{
  array[n_rand] real<lower=0.0> sigma;
  vector[n_rand] steepness;
  matrix[n_rand, K] cumwinprobs;

  for (r in 1:n_rand) {
    sigma[r] = sigma_raw[r]/diff_f;
    cumwinprobs[r, ] = to_row_vector(cum_winprob(to_vector(EloStart[r, ]), k[r], N, K, winner[, r], loser[, r]));
    steepness[r] = cumwinprob2steep(to_vector(cumwinprobs[r, ]), K)[2];
  }
}

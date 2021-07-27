functions {
  vector props2nds(vector props, data int n_ids) {
    matrix[n_ids, n_ids] propmat;
    int cnt;
    vector[n_ids] w;
    vector[n_ids] w2;
    vector[n_ids] l;
    vector[n_ids] l2;
    vector[n_ids] ds;
    vector[n_ids] nds;

    cnt = 1;
    propmat[n_ids, n_ids] = 0.0;
    for (i in 1:(n_ids - 1)) {
      propmat[i, i] = 0.0;
      for (j in 1:i) {
        propmat[j, i + 1] = inv_logit(props[cnt]);
        propmat[i + 1, j] = 1.0 - inv_logit(props[cnt]);
        cnt = cnt + 1;
      }
    }

    for (i in 1:n_ids) {
      w[i] = sum(propmat[i, ]);
      w2[i] = 0.0;
      l[i] = sum(propmat[, i]);
      l2[i] = 0.0;
    }

    for (i in 1:n_ids) {
      for (j in 1:n_ids) {
        w2[i] = w2[i] + w[j] * propmat[i, j];
        l2[i] = l2[i] + l[j] * propmat[j, i];
      }
    }

    for (i in 1:n_ids) {
      ds[i] = w[i] + w2[i] - l[i] - l2[i];
      nds[i] = ((ds[i] + (n_ids * (n_ids - 1.0))/2.0)) / n_ids;
    }

    return nds;
  }

  vector nds2steep(vector nds, data int n_ids) {
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
  int<lower=0> N; // number of interactions
  int<lower=0> K; // number of dyads
  int I; // number of individuals
  int interactions[N]; // interactions, 1/0
  int dyad[N]; // actual dyad
}

parameters{
  vector[K] a; // dyadic winning proportions
}

model {
  vector[N] p;
  vector[I] nds;

  a ~ normal(0, 3); // corresponds to prob of inv_logit(0) = 0.5

  for (i in 1:N) {
    p[i] = inv_logit(a[dyad[i]]);
  }
  interactions ~ binomial(1 , p);

  nds = props2nds(a, I);
}

generated quantities {
  vector[I] normds;
  vector[2] xsteep;
  
  normds = props2nds(a, I);
  xsteep = nds2steep(normds, I);
}

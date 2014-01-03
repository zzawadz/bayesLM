densityStudnet = function(x, df, mu, prec)
{
  (gamma((df+1)/2)/gamma(df/2)) * (prec/pi/df)^(0.5)*(1+(prec*(x-mu)^2)/df)^(-(df+1)/2)
}

real vezes(real r, inteiro x) {
  return r * x;
}

int main()
{
  inteiro x = 2;
  real y = 3.0;
  real z = vezes(y, x);
  printf("multiplicando %d e %.2f ", x, y);
  printf("temos:\n");
  printf("resposta: %f\n", z);
  
  return 0;
}
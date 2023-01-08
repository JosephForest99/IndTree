#include<R.h>
#include<Rmath.h>

// Función para calcular las distancias euclidiana
double dist_euclideana(double x1, double y1, double x2, double y2)
{
  
  double dx, dy;
  
  dx = x2 - x1;
  dy = y2 - y1;
  
  return(sqrt(dx*dx + dy*dy));
  
}

// Función para obtener las distancias entre los árboles
void NN_distances(int *n_, int *id, double *sp, double *size, double *x1, double *y1, double *d, double *vsp, int *vnsp, double *vsize)
{
  int n = *n_;
  
  for (int j = 0; j < n; j++)
  {
    for (int i = 0; i < n; i++)
    {
      if (j != i)
      {
        // Se obtiene el vector de distancias
        d[j*n+i] = dist_euclideana(x1[j], y1[j], x1[i], y1[i]);
        
        // Se obtiene el vector de las especies
        vsp[j*n+i] = sp[i];
        
        // Se obtiene el vector de los id's
        vnsp[j*n+i] = id[i];
        
        // Se obtiene el vector de los tamaños (diámetros)
        vsize[j*n+i] = size[i];
      }
      else
      {
        // Se obtiene el vector de distancias
        d[j*n+i] = 0;
        
        // Se obtiene el vector de las especies
        vsp[j*n+i] = sp[i];
        
        // Se obtiene el vector de los id's
        vnsp[j*n+i] = id[i];
        
        // Se obtiene el vector de los tamaños (diámetros)
        vsize[j*n+i] = size[i];
      }
      
    }
    
  }
  
}

// Función para intercambiar valores
void cambiar(double *px, double *py)
{
  double tmp;    // Se declara una variable temporal para realizar el cambio
  tmp = *px;  // Desreferencia y asigna
  *px = *py;  // Desreferencia y asigna a la variable a la que apunta px
  *py = tmp;  // Desreferencia y asigna a la variable a la que apunta py
  
}

// Función para intercambiar valores
void cambiar2(int *px, int *py)
{
  int tmp;    // Se declara una variable temporal para realizar el cambio
  tmp = *px;  // Desreferencia y asigna
  *px = *py;  // Desreferencia y asigna a la variable a la que apunta px
  *py = tmp;  // Desreferencia y asigna a la variable a la que apunta py
  
}

// Función para ordenar los vectores con el método de insercción
void ordenar_vectores(int *n_, int *id, double *sp, double *size, double *x1, double *y1, double *d, double *vsp, int *vnsp, double *vsize)
{
  int n = *n_;
  
  NN_distances(&n, id, sp, size, x1, y1, d, vsp, vnsp, vsize);
  
  for (int j = 0; j < n; j++)
  {
    for (int i = 1; i < n; i++)
    {
      int ipiv = i;
      
      while (ipiv > 0)
      {
        if (d[j*n+ipiv] < d[j*n+(ipiv-1)])
        {
          cambiar(&d[j*n+ipiv], &d[j*n+(ipiv-1)]);
          cambiar(&vsp[j*n+ipiv], &vsp[j*n+(ipiv-1)]);
          cambiar2(&vnsp[j*n+ipiv], &vnsp[j*n+(ipiv-1)]);
          cambiar(&vsize[j*n+ipiv], &vsize[j*n+(ipiv-1)]);
          ipiv--;
        }
        else
        {
          ipiv = 0;
        }
        
      }
      
    }
    
  }
  
}

// Función para obtener las especies, id's y distancias de los k-ésimos árboles más cercanos
void vecinos_cercanos(int *n_, int *k_, int *id, double *sp, double *size, double *x1, double *y1, double *d, double *vsp, int *vnsp, double *vsize, double *d1, double *vsp1, int *vnsp1, double *vsize1)
{
  
  int n = *n_;
  int k = *k_;
  
  ordenar_vectores(&n, id, sp, size, x1, y1, d, vsp, vnsp, vsize);
  
  for (int j = 0; j < n; j++)
  {
    for (int i = 0; i <= k; i++)
    {
      // Se recuperan los valores en nuevos vectores
      d1[j*n+i] = d[j*n+i];
      vsp1[j*n+i] = vsp[j*n+i];
      vnsp1[j*n+i] = vnsp[j*n+i];
      vsize1[j*n+i] = vsize[j*n+i];
      
    }
    
  }
  
}

// Función para calcular el índice de mezcla de especies
void mezcla_indice_arbol(int *n_, int *k_, int *id, double *sp, double *size, double *x1, double *y1, double *d, double *vsp, int *vnsp, double *vsize, double *d1, double *vsp1, int *vnsp1, double *vsize1, double *indice)
{
  
  int n = *n_;
  int k = *k_;
  int suma;
  
  vecinos_cercanos(&n, &k, id, sp, size, x1, y1, d, vsp, vnsp, vsize, d1, vsp1, vnsp1, vsize1);    
  
  for (int j = 0; j < n; j++)
  {
    // Se inicializa la suma en 0 para el árbol "j"
    suma = 0;
    
    for (int i = 1; i <= k; i++)
    {
      if (vsp1[j*n+0] != vsp1[j*n+i])
      {
        suma = suma + 1;
      }
      
    }
    
    // Se obtiene el índice de mezcla de especies para el árbol "j"
    indice[j] = (double) suma / k;
    
  }    
  
}


// Función para obtener el valor mínimo de dos números
double minimo(double x1, double x2)
{
  if (x1 < x2)
  {
    return x1;
  }
  else
  {
    return x2;
  }
  
}

// Función para obtener el valor maximo de dos números
double maximo(double x1, double x2)
{
  if (x1 > x2)
  {
    return x1;
  }
  else
  {
    return x2;
  }
  
}

// Función para obtner el índice de diferenciación dimensional
void tamano_indice_arbol(int *n_, int *k_, int *id, double *sp, double *size , double *x1, double *y1, double *d, double *vsp, int *vnsp, double *vsize, double *d1, double *vsp1, int *vnsp1, double *vsize1, double *indice)
{
  int n = *n_;
  int k = *k_;
  double suma, r;
  
  vecinos_cercanos(&n, &k, id, sp, size, x1, y1, d, vsp, vnsp, vsize, d1, vsp1, vnsp1, vsize1);
  
  for (int j = 0; j < n; j++)
  {
    
    suma = 0;
    
    for (int i = 1; i <= k; i++)
    {
      r = 1 - (minimo(vsize1[j*n+0], vsize1[j*n+i]) / maximo(vsize1[j*n+0], vsize1[j*n+i]));
      
      suma = suma + r;
    }
    
    indice[j] = suma / k;
    
  }
  
}

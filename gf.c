/*  GF(2^8), and x is primitive element.
    primitive polynomial: x^8+x^4+x^3+x^2+1, [1 0001 1011] 
        Generated element: {0, x^0, x^1, x^2, ...,x^i,..., x^255}, 'i' represents generated element
        Polynomial element: (0, 1, x, x^2, .., x7, x^4+x^3+x^2+1, ..., a,...), 'a' represents polynomial element
    ------------------
    for each i(i > 1)
        x^i = x*(x^(i-1))
    ------------------
    author: Dongsheng Wei
    email:  12210240069@fudan.edu.cn
*/
#include <stdio.h>
#include <stdlib.h>
int gfilog[256];    //Generated element: gfilog[i] = a, a = x^i
int gflog[256];    //polynomial element: gflog[a] = i, i = log(x)(a)
//typedef gfilog gfilog;
//typedef gflog gflog;
void genlt()
{
    int i, j;
    
    gfilog[0] = 1;  //x^0 = 1
    for (i = 1; i < 256; i++){
        j = gfilog[i-1] << 1;   //for each i, x^i = x*(x^(i-1))
        if ((j & 0x100) != 0)
            j ^= 0x11D;        
        gfilog[i] = j;
    }

    gflog[0] = gflog[1] = 0;
    for (i = 1; i < 255; i++)    
        gflog[gfilog[i]] = i;
}
int my_mul( int a, int b )
{
    if( a==0 || b==0 ) 
        return 0;
    else    
        return gfilog[ (gflog[a]+gflog[b])%255 ];
}
int my_div( int a, int b )
{
    int x;
    if( a==0 )  return 0;
    if( b==0 )  return -1;
    x = gflog[a]-gflog[b] < 0 ? gflog[a]-gflog[b]+255 : gflog[a]-gflog[b];
    return gfilog[x];
}
void main()
{
    genlt();
    int i;
    unsigned char DEF = 2;
    unsigned char C[256], M[256], O[256];
    sprintf( (char*)C, "TMD" );
    printf( "%s\n", C );
    for( i=0; i<256; i++ ){
        M[i] = mul( DEF, C[i] );
    }
    printf( "%s\n", M );
    for( i=0; i<256; i++ ){
        O[i] = my_div( M[i], DEF );
    }
    printf( "%s\n", O );
}
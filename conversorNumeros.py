#Este es un programa que convierte números indoarábigos en números romanos.
#Matriz donde se insertarán los resultados de todas las funciones.
rom=[]

#Función que toma el número y, dependiendo de si tiene unidades de millar/centenas/decenas/unidades ejecuta las funciones correspondientes.
#Las funciones se ejecutan de la unidad más grande a la más pequeña y después de cada función, se le restan dichas unidades.
#Ejemplo: Número:2345 --> función "millares" --> se le resta 1000 dos veces --> Número actual:345
#          --> función "centenas" --> se le resta 100 tres veces --> Número actual:45 --> función "decenas" --> se le resta 10 cuatro veces...
def ajuste(res):
    if res>=1000:
        millares(res)
        while res>=1000:
            res=res-1000
    if res>=100:
        centenas(res)
        while res>=100:
            res=res-100
    if res>10:
        decenas(res)
        while res>=10:
            res=res-10
    if res>=1:
        unidades(res)
    
#Función que convierte las unidades de millar del número arábigo a romano. Primero se hace una división de enteros entre 1000 para obtener
#únicamente las unidades de millar del número original. Si el número es menor a 1 o mayor a 4, la función retorna a la funcion "ajuste"; si
#el número está entre 1 y 3, un ciclo while escribe M por cada unidad de millar que hay. El resultado se insertará en una lista, que a su
#vez se insertará en la matriz "rom".
def millares(mil):
    mil=mil//1000
    count1=[]
    if mil>=1 and mil<4:
        i=0
        while i < mil:
            count1.append("M")
            i+=1
        rom.append(count1)
    else:
        return

#Función que convierte las centenas del número arábigo a romano. Primero se hace una división de enteros entre 100 para obtener únicamente
#las centenas del número original. Si el número es 4 o 9, se escribe su equivalente en romano; si el número es menor a 4, un ciclo while
#escribe C por cada centena del número; si es mayor o igual a 5, se escribe D, se le resta 5 al número y un ciclo while escribe C por cada 
#centena que queda. El resultado se inertará en una lista, que a su vez se insertará en la matriz "rom".
def centenas(cen):
    cen=cen//100
    count2=[]
    if cen==4:
        count2.append("CD")
        rom.append(count2)
    elif cen==9:
        count2.append("CM")
        rom.append(count2)
    else:
        i=0
        if cen-5>=0:
            cen=cen-5
            count2.append("D")
            while i<cen:
                count2.append("C")
                i+=1
            rom.append(count2)
        else:
            while i<cen:
                count2.append("C")
                i+=1
            rom.append(count2)

#Función que convierte las decenas del número arábigo a romano. Primero se hace una división de enteros entre 10 para obtener únicamente
#las decenas del número original. Si el número es 4 o 9, se escribe su equivalente en romano; si el número es menor a 4, un ciclo while
#escribe X por cada centena del número; si es mayor o igual a 5, se escribe L, se le resta 5 al número y un ciclo while escribe X por cada
#decena que queda. El resultado se insertará en una lista, que a su vez se insertará en la matriz "rom".
def decenas(dec):
    dec=dec//10
    count3=[]
    if dec==4:
        count3.append("XL")
        rom.append(count3)
    elif dec==9:
        count3.append("XC")
        rom.append(count3)
    else:
        i=0
        if dec-5>=0:
            dec=dec-5
            count3.append("L")
            while i<dec:
                count3.append("X")
                i+=1
            rom.append(count3)
        else:
            while i<dec:
                count3.append("X")
                i+=1
            rom.append(count3)
            
#Función que convierte las unidades del número arábigo a romano. Si el número es 4 o 9, se escribe su equivalente en romano; si el número es
#menor a 4, un ciclo while escribe I por cada centena del número; si es igual o mayor a 5, se escribe V, se le resta 5 al número y un ciclo
#while escribe I por cada unidad que queda. El resultado se inserta en una lista, que a su vez se inserta en la matriz "rom"
def unidades(uni):
    count4=[]
    if uni==4:
        count4.append("IV")
        rom.append(count4)
    elif uni==9:
        count4.append("IX")
        rom.append(count4)
    else:
        i=0
        if uni-5>=0:
            uni=uni-5
            count4.append("V")
            while i<uni:
                count4.append("I")
                i+=1
            rom.append(count4)
        else:
            while i<uni:
                count4.append("I")
                i+=1
            rom.append(count4)

#Display del menú. Después de ejecutar la función que convierte el número arábigo en romano, se ejecuta un ciclo for para convertir los
#números de la matriz "rom" en texto normal, retirando los corchetes y comas que separan a los resultados de cada función.

def main():
    print("Conversor de números arábigos a romanos")
    num=int(input("Ingresa un número: "))
    ajuste(num)
    dis=''
    for fila in rom:
        for elemento in fila:
            dis=dis+elemento
    print(dis)
        
    

main()


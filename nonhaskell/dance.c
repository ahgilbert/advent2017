#include<stdio.h>
const int SIZE=16;

// abcdefghijklmnop
// 0123456789012345
// iojhdbnmklgceapf
void fixedSwap(char* arr1, char* arr2){
   arr2[0] = arr1[8];
   arr2[1] = arr1[14];
   arr2[2] = arr1[9];
   arr2[3] = arr1[7];
   arr2[4] = arr1[3];
   arr2[5] = arr1[1];
   arr2[6] = arr1[13];
   arr2[7] = arr1[12];
   arr2[8] = arr1[10];
   arr2[9] = arr1[11];
   arr2[10] = arr1[6];
   arr2[11] = arr1[2];
   arr2[12] = arr1[4];
   arr2[13] = arr1[0];
   arr2[14] = arr1[15];
   arr2[15] = arr1[5];
}

// abcdefghijklmnop
// mendpalcbghoifkj
void charSwaps(char* arr1, char* arr2){
    for (int i = 0; i < SIZE; i++) {
        switch(arr1[i]) {
            case 'a':
                arr2[i] = 'm';
                break;
            case 'b':
                arr2[i] = 'e';
                break;
            case 'c':
                arr2[i] = 'n';
                break;
            case 'd':
                arr2[i] = 'd';
                break;
            case 'e':
                arr2[i] = 'p';
                break;
            case 'f':
                arr2[i] = 'a';
                break;
            case 'g':
                arr2[i] = 'l';
                break;
            case 'h':
                arr2[i] = 'c';
                break;
            case 'i':
                arr2[i] = 'b';
                break;
            case 'j':
                arr2[i] = 'g';
                break;
            case 'k':
                arr2[i] = 'h';
                break;
            case 'l':
                arr2[i] = 'o';
                break;
            case 'm':
                arr2[i] = 'i';
                break;
            case 'n':
                arr2[i] = 'f';
                break;
            case 'o':
                arr2[i] = 'k';
                break;
            case 'p':
                arr2[i] = 'j';
                break;
        }
    }
    return;
}


void dance(char* arr1, char* arr2) {
    charSwaps(arr2,arr1);
    fixedSwap(arr1, arr2);
}

void initArray(char* arr) {
    for(int i = 0; i < SIZE; i++) {
        arr[i] = i + 97;
    }
    arr[SIZE] = '\n';
    arr[SIZE+1] = '\0';
    return;
}

int main()
{
    char arr1[SIZE + 2];
    char arr2[SIZE + 2];
    initArray(arr1);
    initArray(arr2);
    for(int i = 0; i < 1000000000; i++) {
        dance(arr1, arr2);
    }
    printf(arr2);

    return 0;
}

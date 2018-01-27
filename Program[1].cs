using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using NeuralNet;


class Program
{
    static void Main(string[] args)
    {
        double[,] input = new double[,] { { 1, 2 }, { 3, 4 }, { 5, 6 } }; //2D input matrix
        NeuralNetSOM som = new NeuralNetSOM(4,5,input.GetLength(1));
        double[,,] codebook = som.Train(input);

        som.SeeCodebook();

        #region see input data
        for (int i=0; i<input.GetLength(0);i++)
        {
            for (int j = 0; j < input.GetLength(1); j++)
            {
                Console.Write(string.Format("{0}\t", input[i, j]));
            }
            Console.Write(Environment.NewLine + Environment.NewLine);
        }
        #endregion

        
    

    }
}


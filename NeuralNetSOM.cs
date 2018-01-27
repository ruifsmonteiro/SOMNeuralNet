using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MathNet.Numerics.Distributions;

namespace NeuralNet
{
    class NeuralNetSOM
    {
        // Field 
        public double[,,] codebook;

        // Constructor 
        public NeuralNetSOM()
        {
            int dim1 = 4, dim2 = 5, dimI = 10; //default dimensions
            codebook = new double[dim1, dim2, dimI]; //dimI -  input data dimension
        }

        public NeuralNetSOM(int dim1, int dim2, int dimI)
        {
            codebook = new double[dim1, dim2, dimI];
        }
    
        //Methods
        public double[,,] Train(double[,] trainset)
        {
            #region initialize random codebook - i'll put zeros by now
            int dim1 = codebook.GetLength(0);
            int dim2 = codebook.GetLength(1);
            int dimI = codebook.GetLength(2);
            //i : index used to sweep across dim1 
            //j : index used to sweep across dim2
            //k : index used to sweep across input data columns and and columns code vectors from codebook
            #endregion

            #region Initialize Random Codebook
            double mean = 0;
            double stdDev = 1;
            Normal normalDist = new Normal(mean, stdDev);
            for (int i = 0; i < dim1; i++)
            {
                for (int j = 0; j < dim2; j++)
                {
                    for (int k = 0; k < dimI; k++)
                    {
                        double randomGaussianValue = normalDist.Sample();
                        codebook[i, j, k] = randomGaussianValue; 
                    }
                }
            }
            #endregion

            #region find BMU
            double[] vsample = null, codevector = null; //initialize a sample vector from training set and a codevector 
            for (int l = 0 ; l < trainset.GetLength(0) ; l++) //mudar e por a passar aleatoriamente um elemento do treino, e por tb o nr de iteraçoes
            {
                for (int k = 0; k < trainset.GetLength(1); k++)
                {
                    vsample[k] = trainset[l, k];
                }

                for (int i = 0; i < codebook.GetLength(0); i++)
                {
                    for (int j = 0; j < codebook.GetLength(1); j++)
                    {
                        for (int k = 0; k < codebook.GetLength(2); k++)
                        {
                            codevector[k] = codebook[i, j, k];
                        }
                    }
                }

                
                //calculate and allocate distances of a selected input row to the neurons
                double[] distances = null;
                distances[l] = Distance(vsample, codevector);

            
            }


            #endregion




                return codebook;
        }

        public void SeeCodebook()
        {
            for (int i = 0; i < codebook.GetLength(0); i++)
            {
                for (int j = 0; j < codebook.GetLength(1); j++)
                {
                    for (int k = 0; k < codebook.GetLength(2); k++)
                    {
                        Console.Write("{0}\t", codebook[i, j, k]);
                    }
                    Console.WriteLine("");
                }

            }
        }

        public void PrintCodebook()
        { /*to be made*/}


        //other methods
        private double Distance(double[] vector1, double[] vector2)
        {
            double value = 0;
            for (int i = 0; i < vector1.Length; i++)
            {
                value += Math.Pow((vector1[i] - vector2[i]), 2);
            }
            return Math.Sqrt(value);
        }

    }
}

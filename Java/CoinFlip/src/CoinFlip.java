//********************************************************************
//  CoinFlip.java      
//
//
//********************************************************************

public class CoinFlip
{
   //-----------------------------------------------------------------
   //  Creates a Coin object, flips it, and prints the results.
   //-----------------------------------------------------------------
   public static void main (String[] args)
   {
      MonetaryCoin coin1 = new MonetaryCoin(1);
      MonetaryCoin coin2 = new MonetaryCoin(5);
      MonetaryCoin coin3 = new MonetaryCoin(10);
      MonetaryCoin coin4 = new MonetaryCoin(25);
      MonetaryCoin coin5 = new MonetaryCoin(50);
      MonetaryCoin coin6 = new MonetaryCoin(100);
      MonetaryCoin coin7 = new MonetaryCoin(100);
      
      

      coin1.flip();
      int value1 = coin1.pay();
      coin2.flip();
      int value2 = coin2.pay();
      coin3.flip();
      int value3 = coin3.pay();
      coin4.flip();
      int value4 = coin4.pay();
      coin5.flip();
      int value5 = coin5.pay();
      coin6.flip();
      int value6 = coin6.pay();
      coin7.flip();
      int value7 = coin7.pay();
      
      System.out.println (coin1 +"\t"+ value1);
      System.out.println (coin2 +"\t"+ value2);
      System.out.println (coin3 +"\t"+ value3);
      System.out.println (coin4 +"\t"+ value4);
      System.out.println (coin5 +"\t"+ value5);
      System.out.println (coin6 +"\t"+ value6);
      System.out.println (coin7 +"\t"+ value7);
      
    System.out.println ();
      
    System.out.println ("Total Value: "+(value1+value2+value3+value4+value5+value6+value7));
      
   }
}
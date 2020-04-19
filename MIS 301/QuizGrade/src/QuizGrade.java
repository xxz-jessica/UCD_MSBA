//********************************************************************
//  QuizGrade.java       Author: Zhang,Xingxuan
//
//  Solution to Assignment 3
//********************************************************************
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.text.DecimalFormat;
import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.CategoryAxis;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.stage.Stage;

public class QuizGrade extends Application {
	//x axis string
	final static String A = "A's 18-20";
    final static String B = "B's 16-17";
    final static String C = "C's 14-15";
    final static String D = "D's 12-13";
    final static String E = "E's 0-11";
    
    
   public void start(Stage primarystage) {
	   // create bar chart 
    	primarystage.setTitle("Grades");
        final CategoryAxis xAxis = new CategoryAxis();
        final NumberAxis yAxis = new NumberAxis();
        final BarChart<String,Number> bc = 
            new BarChart<String,Number>(xAxis,yAxis);
        bc.setTitle("Bar Graph of Student Grades");
        xAxis.setLabel("Score Range");       
        yAxis.setLabel("Number of Students");

        XYChart.Series series1 = new XYChart.Series();
        series1.setName(null);       
        series1.getData().add(new XYChart.Data(A, 51));
        series1.getData().add(new XYChart.Data(B, 23));
        series1.getData().add(new XYChart.Data(C, 15));
        series1.getData().add(new XYChart.Data(D, 3));
        series1.getData().add(new XYChart.Data(E, 8)); 
        
        Scene scene  = new Scene(bc,600,600);
        bc.getData().addAll(series1);
        primarystage.setScene(scene);
        primarystage.show();
        }
     
    
	public static void main (String[] args) throws IOException
	{
//read files
		File file = new File ("C:\\SRing 2018\\MIS 301\\Assignment 3\\questionanswers.txt");
		File file1 = new File ("C:\\SRing 2018\\MIS 301\\Assignment 3\\studentanswers.txt");
		BufferedReader br = new BufferedReader(new FileReader(file));
		BufferedReader br1 = new BufferedReader(new FileReader(file1)); 
		
		boolean [] Answers = new boolean [20];//array for correct answers
		final int QUESTION = 20;
		int i = 0;
		
		for (i = 0; i<QUESTION;i++)
		{
			Answers [i]= Boolean.valueOf(br.readLine());
		}
		
		
		boolean [][] Questionanswers = new boolean [100][20];//array for 100 students'20 questions
		final int STUDENT = 100;
		final int ANSWER = 20;
		int x = 0;
		int y = 0;
		
		 for (x = 0; x<STUDENT;x++) 
		 {for (y = 0;y<ANSWER;y++)
			 {Questionanswers [x][y] = Boolean.valueOf(br1.readLine());
			 }
		 }
		 
		/* System.out.println("The Matrix of Answers");
		 System.out.println();
		 for (x = 0; x<STUDENT;x++)
		 {for (y = 0;y<ANSWER;y++)
			 System.out.print(Questionanswers[x][y]+" ");
		 System.out.println();
		 }*/
   
	int Score []=new int [100]; //array for the score
	
	int score= 0;
	for (x = 0; x<STUDENT; x++)
	{
		for (y = 0;y<ANSWER;y++)
		{
			if (Questionanswers[x][y]==Answers[y])
				score = score+1;
			
		}
		Score [x]= score;
		score = 0;
		

	}
	int Count []= new int[20];//array for number of students who answer the question right
	int count = 0;
	
	for (y = 0; y<ANSWER; y++)
	{
		for (x = 0;x<STUDENT;x++)
		{
			if (Questionanswers[x][y]==Answers[y])
				count = count+1;
		}
		Count [y]= count;
		count = 0;
	}
	
	System.out.println("STUDENT SCORES");
	System.out.println();
	
	final int PER_LINE = 4;
	int line = 0;
	int line2 = 0;
	
	for (x = 0; x<STUDENT;x++)//print the score for each student
	{
		System.out.print("Student #"+(x+1)+": ");
		System.out.print(Score[x]+"\t\t");
		line++;
		if (line%PER_LINE ==0)
			System.out.println();
	}
	System.out.println();
	System.out.println("NUMBER OF STUDENTS WHO ANSWERED QUESTION CORRECTLY");
	
	System.out.println();
	
	for (y = 0; y<ANSWER; y++)//print the number of students who answer the question right
	{
		System.out.print("Question #"+(y+1)+": ");
		System.out.print(Count[y]+"\t\t");
		line2++;
		if (line2 % PER_LINE == 0)
			System.out.println();
	}
	double sumscore = 0;//calculate the average score
	for (x=0;x<STUDENT;x++)
	{
		sumscore +=Score[x];
		
	}
	System.out.println();
	double average = sumscore/STUDENT;
	System.out.println("THE AVERAGE GRADE IS: "+ average);
	
	DecimalFormat fmt = new DecimalFormat ("0.###");//calculate the standard deviation
	
	double standardDeviation = 0;
	for (int num: Score)
	{
		standardDeviation += Math.pow(num - average, 2);
	}
	standardDeviation = Math.sqrt(standardDeviation/STUDENT);
	System.out.println("THE STANDARD DEVIATION OF STUDENT GRADES IS: "+ fmt.format(standardDeviation));
	
	
    int As = 0;//calculate the number of students in each score range
	 int Bs = 0;
	 int Cs = 0;
	 int Ds = 0;
	 int Es = 0;
	
	for (x=0; x<STUDENT;x++)
	{
		if (Score[x]>0)
		{
			if (Score [x]>11)
			{
				if(Score[x]>13)
				{
					if (Score[x]>15)
					{
						if (Score[x]>17)
							As++;
						else
							Bs++;
					}
					else 
						Cs++;
				}
				else
					Ds++;
			}
			else
				Es++;
		}
		
	}
	
	
	
	launch(args);
}
   
    
	
}


		



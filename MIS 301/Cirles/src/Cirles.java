import javafx.stage.Stage;


import java.util.Random;
import javafx.geometry.Pos;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.Group;
import javafx.scene.paint.Color;



import javafx.application.Application;

public class Cirles extends Application
{
	public void start(Stage primaryStage)
	{
		
		Group root = new Group ();
		Random gen = new Random ();
		
		for (int count = 1;count <=20;count++)
		{
			int x = gen.nextInt(350)+1;
			int y = gen.nextInt(350)+1;
			int radius = gen.nextInt(100)+1;
			
			Color fill = null;
			
			
		}
		
		Scene scene = new Scene (root, 400, 400);
		
		primaryStage.setTitle("Circle Overlap");
		primaryStage.setScene(scene);
		primaryStage.show();
		
		
	}
	public static void main(String[] args)
	{
		launch(args);
	}
}

//********************************************************************
//  SlotMachine1.java       Author: Zhang,Xingxuan
//
//  Solution to Individual practice
//********************************************************************

//-----------------------------------------------------------------
	   //  this is a driver code for slot machine.
	   //-----------------------------------------------------------------



import javafx.application.Application;
import javafx.geometry.Pos;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

	


public class SlotMachine1 extends Application
{
	public void start(Stage primaryStage)
	{
		SlotMachinePane1 pane = new SlotMachinePane1 ();//create a slot machine pane object
		
		
		Scene scene = new Scene (pane, 1000, 500);
		
		primaryStage.setTitle("SlotMachine");
		primaryStage.setScene(scene);
		primaryStage.show();
	}
	public static void main(String[] args)
	{
		launch(args);
	}
}


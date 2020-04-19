import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.FlowPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import java.util.Random;

public class SlotMachinePane extends VBox{
	private FlowPane buttonPane, newSlotPane;
	private VBox primary;
	//-------------------------------
	//Constructor for slot machine pane
	//-------------------------------
	public SlotMachinePane () {
		newSlotPane = new FlowPane ();
		newSlotPane.setPrefSize(800, 250);
		newSlotPane.setStyle("-fx-background-color: white");
		
		buttonPane = new FlowPane();
		buttonPane.setStyle("-fx-background-color: blue");
		buttonPane.setPrefSize(800, 250);
	
		
		primary = new VBox ();
		
		primary.setStyle("-fx-background-color: red");
		primary.setAlignment(Pos.CENTER);
		primary.getChildren().addAll(newSlotPane, buttonPane); //nest other two panes
		primary.setPrefSize(1000, 500);
		primary.setPadding(new Insets(50));
		
		getChildren().addAll(primary);
	}
	
	
}


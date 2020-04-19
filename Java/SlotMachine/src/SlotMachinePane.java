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
import javax.swing.ImageIcon;
import javax.swing.JOptionPane;

public class SlotMachinePane extends VBox{
	private FlowPane buttonPane, newSlotPane;
	private VBox primary;
	private Label spinLabel, tokenLabel,textLabel;
	private Button spinButton,cashButton;
	private Image imageLabel;
	private int line,count,value;
	private ImageView imageView;
	
	//-------------------------------
	//Constructor for slot machine pane
	//-------------------------------
	public SlotMachinePane () {
		count = 5;
		value = 0;

		spinLabel = new Label("Result of spin: "+ value);
		spinLabel.setFont(Font.font("Helvetcia", FontWeight.LIGHT, 40));
		spinLabel.setTextFill(Color.BLACK);
		
		tokenLabel = new Label("Current token: " + count);
		tokenLabel.setFont(Font.font("Helvetcia", FontWeight.LIGHT, 40));
		tokenLabel.setTextFill(Color.BLACK);
		
		textLabel = new Label("NO Button Pushed");
		textLabel.setFont(Font.font("Helvetcia", FontWeight.BOLD, 40));
		textLabel.setTextFill(Color.WHITE);
		
		spinButton = new Button("Spin");
		spinButton.setPrefSize(250, 80);
		spinButton.setFont(Font.font("Arial", FontWeight.NORMAL, 40));
		spinButton.setOnAction(this::processButtonPress);
		
		cashButton = new Button("Cash Out");
		cashButton.setPrefSize(250, 80);
		cashButton.setFont(Font.font("Arial", FontWeight.NORMAL, 40));
		cashButton.setOnAction(this::processButtonPress);
		
		imageLabel = new Image ("aaa.png");
		imageView = new ImageView (imageLabel);
		
		
		
		newSlotPane = new FlowPane (tokenLabel,spinLabel,imageView);
		newSlotPane.setHgap(100);
		newSlotPane.setVgap(30);
		newSlotPane.setAlignment(Pos.CENTER);
		newSlotPane.setPrefSize(800, 250);
		newSlotPane.setStyle("-fx-background-color: white");
		
		buttonPane = new FlowPane(spinButton,cashButton,textLabel);
		buttonPane.setHgap(100);
		buttonPane.setAlignment(Pos.CENTER);
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
	public void processButtonPress (ActionEvent event) {
		textLabel.setText("A Button Was Pushed!");
		Random gen = new Random ();
		line = gen.nextInt(27);
		if (count>=0)
		{
		
			
			if (event.getSource() == spinButton) 
			{
				
				switch (line)
				{
				case 0:
				Image icon = new Image ("aaa.png");
				imageView.setImage (icon);
				value = 5;
				break;
				case 1:
				Image icon1 = new Image ("aao.png");
				imageView.setImage (icon1);
				value = 1;
				break;
				case 2:
				Image icon2 = new Image ("aac.png");
				imageView.setImage (icon2);
				value = 4;
				break;
				case 3:
					tokenLabel.setText("Current token: "+ count);
					Image icon3 = new Image ("aca.png");
					imageView.setImage (icon3);
					value = 2;
					break;
				case 4:
					tokenLabel.setText("Current token: "+ count);
					Image icon4 = new Image ("acc.png");
					imageView.setImage (icon4);
					value = 3;
					break;
				case 5:
						tokenLabel.setText("Current token: "+ count);
						Image icon5 = new Image ("aco.png");
						imageView.setImage (icon5);
						value = 0;
						break;
					case 6:
						tokenLabel.setText("Current token: "+ count);
						Image icon6 = new Image ("aoa.png");
						imageView.setImage (icon6);
						value = 1;
						break;
					case 7:
						tokenLabel.setText("Current token: "+ count);
						Image icon7 = new Image ("aoc.png");
						imageView.setImage (icon7);
						value = 0;
						break;
					case 8:
						tokenLabel.setText("Current token: "+ count);
						Image icon8 = new Image ("aoo.png");
						imageView.setImage (icon8);
						value = 0;
						break;
					case 9:
						tokenLabel.setText("Current token: "+ count);
						Image icon9 = new Image ("caa.png");
						imageView.setImage (icon9);
						value = 3;
						break;
					case 10:
						tokenLabel.setText("Current token: "+ count);
						Image icon10 = new Image ("cac.png");
						imageView.setImage (icon10);
						value = 2;
						break;
					case 11:
						tokenLabel.setText("Current token: "+ count);
						Image icon11 = new Image ("cao.png");
						imageView.setImage (icon11);
						value = 0;
						break;
					case 12:
						tokenLabel.setText("Current token: "+ count);
						Image icon12 = new Image ("cca.png");
						imageView.setImage (icon12);
						value = 4;
						break;
					case 13:
						tokenLabel.setText("Current token: "+ count);
						Image icon13 = new Image ("ccc.png");
						imageView.setImage (icon13);
						value = 10;
						break;
					case 14:
						tokenLabel.setText("Current token: "+ count);
						Image icon14 = new Image ("cco.png");
						imageView.setImage (icon14);
						value = 2;
						break;
					case 15:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon15 = new Image ("coa.png");
						imageView.setImage (icon15);
						value = 0;
						break;
					case 16:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon16 = new Image ("coc.png");
						imageView.setImage (icon16);
						value = 2;
						break;
					case 17:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon17 = new Image ("coo.png");
						imageView.setImage (icon17);
						value = 0;
						break;
					case 18:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon18 = new Image ("oaa.png");
						imageView.setImage (icon18);
						value = 1;
						break;
					case 19:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon19 = new Image ("oac.png");
						imageView.setImage (icon19);
						value = 0;
						break;
					case 20:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon20 = new Image ("oao.png");
						imageView.setImage (icon20);
						value = -1;
						break;
					case 21:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon21 = new Image ("oca.png");
						imageView.setImage (icon21);
						value = 0;
						break;
					case 22:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon22 = new Image ("occ.png");
						imageView.setImage (icon22);
						value = 1;
						break;
					case 23:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon23 = new Image ("oco.png");
						imageView.setImage (icon23);
						value = -1;
						break;
					case 24:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon24 = new Image ("ooa.png");
						imageView.setImage (icon24);
						value = 0;
						break;
					case 25:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon25 = new Image ("ooc.png");
						imageView.setImage (icon25);
						value = 0;
						break;
					case 26:
						
						tokenLabel.setText("Current token: "+ count);
						Image icon26 = new Image ("ooo.png");
						imageView.setImage (icon26);
						value = -50;
						break;
				}
				count = count + value;
				count = count- 1;
				tokenLabel.setText("Current token: "+ count);
				spinLabel.setText("Result of spin: "+ value);
				
			}
			
				
		}
		
		else {
			
			textLabel.setText("Game Over!!!!!!!!");
			int res=JOptionPane.showConfirmDialog(null,"Do you want to play again?", "choose one",JOptionPane.YES_NO_OPTION);
		 	if(res==JOptionPane.YES_OPTION){
		 		count = 5;
		 		value = 0;
		 		tokenLabel.setText("Current token: "+ count);
		 		spinLabel.setText("Result of spin: "+ value);
		 	}
		 	else{
		 		System.exit(0);
		 		return;
		 	}
		}
		
	}
	
	
}


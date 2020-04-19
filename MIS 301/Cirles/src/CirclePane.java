import java.awt.Color;
import java.awt.Graphics;
import java.util.Random;

public class CirclePane {
	private final int MAXCIRCLE = 20;
	Circles[] circle = new Circles[MAXCIRCLE]; //declares array of 20 circle objects
	    Random gen = new Random();
	    Color circleColor;

	    private int[] randX = new int[MAXCIRCLE];  //declares arrays of x,y,and radius
	    private int[] randY = new int[MAXCIRCLE];

	    private int[] radius = new int[MAXCIRCLE];

	    public CirclePane() {
	
	        setBackground(Color.lightGray);

	        for(int x =0; x < MAXCIRCLE; x++){ //counts through arrays

	            randX[x] += gen.nextInt(600)+1; //adds random numbers to arrays

	            randY[x] += gen.nextInt(400)+1;
	
	            radius[x] += gen.nextInt(201)+40;
	
	            circle[x] = new Circles(radius[x],circleColor,randX[x],randY[x]); //declares new array object with instance variables

	        }

	    }
	    

	    public void paint(Graphics g) {
	
	        super.paint(g);
	        for (int x =0; x< randX.length; x++) {          
	            for (int i =0; i< randY.length; i++)//loops through and checks if circle are overlapping..if they are sets color to cyan
	            {
		                if(Math.sqrt((randX[x]-randX[i])*(randX[x]-randX[i])+(randY[x]-randY[i])*(randY[x]-randY[i])) > radius[x]-radius[i])
	
	                {  
	
	                    circle[x].setColor(Color.cyan);
	
	                    circle[x].draw(g);

	                    repaint();

	                }

	                circle[x].setColor(Color.black);

	                circle[x].draw(g);

	                repaint();
	         }
	
	        }
	        
	    }
	    public class Circles {
	    	private int x, y, radius;
	    	private Color color;
	    	
	    	public Circles (int size, Color circlecolor, int X, int Y)
	    	{		
	    		radius = size;
	    		color = circlecolor;
	    		x = X;
	    		y = Y;
	    	}
	    	public void draw (Graphics page)
	    	{
	    		page.setColor (color);
	    		page.fillOval (x, y, radius, radius);
	    	}
	    	public Color getColor() {
	    		return color;
	    	}

	    	public void setColor(Color color) {
	    		this.color = color;
	    	}	
	        

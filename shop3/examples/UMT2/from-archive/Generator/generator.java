import java.io.*;
import java.util.Random;

public class generator{
    
    // need two input for the program:
        //    a. Number of Packages
        //    b. Number of Problems
        
    public static void main(String args[]){
        
        int Package_Num;
        int Problem_Num;
        
        if(args.length != 2){
            System.out.println("need two parameters: Number of Packages and Number of Problems");
            return;
        }else{
            try{
                Package_Num = Integer.parseInt(args[0]);    
            }catch(NumberFormatException e){
                System.out.println("need integer for package number");
                return;
            }
            
            try{
                Problem_Num = Integer.parseInt(args[1]);    
            }catch(NumberFormatException e){
                System.out.println("need integer for problem number");
                return;
            }
            
            problem_set a = new problem_set(Package_Num, Problem_Num);
            
            
            a.generate();
        
        }
    }
}
    
class problem_set{
    
    // problem number
    int Problem_Num;
    // package number;
    int Package_Num;
    // region number
    int Region_Num;
    // city number
    int City_Num;
    // location number
    int Location_Num;
    // number of trucks
    int truck;
    // number of trains
    int train;
    // number of airplanes
    int airplane;
    
    // upper bound of 
    // a. Distance between two locations
    int Distance_UB = 70;
    // b. Height Capacity
    int Height_Cap_UB = 30;
    // c. Length Capacity
    int Length_Cap_UB = 40;
    // d. Width Capacity
    int Width_Cap_UB = 30;
    // e. Weight Capacity
    int Weight_Cap_UB = 500;
    // f. Volume Capacity
    int Volume_Cap_UB = 500; 
    // g. Gas inside a Vehicle
    int Gas_UB = 500;
    // h. Gas per Mile for a Vehicle
    int Gpm_UB = 3;
    // i. Height, Length, Width and Weight of a Vehicle 
    int Height_UB = 10;
    int Length_UB = 15;
    int Width_UB = 10;
    int WeightV_UB = 30;
    // j. Weight and Volume of a Package
    int WeightP_UB = 20;
    int VolumeP_UB = 20;
    
    // all possible vehicle type
    String[] v_type = {"regularv", "flatbed", "tanker", "auto", "hopper"};
    // all possible package type
    String[] p_type = {"regularp", "bulky", "liquid", "cars", "mail", "granular"};
    
    // package type used
    int type_p;
    // vehicle type used
    int type_v;
    
    // number of cranes
    int Crane_Num;
    // number of plane ramps
    int Ramp_Num;
    
    RandomAccessFile Pddl;
    RandomAccessFile Pddl_Temp;
    
    Random random = new Random();
    
    // keep track a city's region number
    int[] Region_City;
    // keep track a location's region number
    int[] Region_Loc;
    
    // keep track which location is a train_station
    int[] train_station;
    // if it is a hub
    boolean[] hub_train;
    // total number of train stations
    int num_t;
    
    // same as above, but for airport
    int[] airport;
    boolean[] hub_air;
    int num_a; 
    
    // keep track the initial volume load of each location
    int[] load;
    
    int [][] Region_Serve_T;
    int [][] Region_Serve_A;
    
    int[] Height_Loc;
    int[] Length_Loc;
    int[] Width_Loc;
    int[] Volume_Loc;
    
    // if there is only one package, remember its location
    int package_loc;
    
    problem_set(int Package_Num, int Problem_Num){
        
        this.Package_Num = Package_Num;
        this.Problem_Num = Problem_Num;
        Region_Num = Math.max(1, Package_Num/2);
        City_Num = Math.max(2, (int)(Package_Num*0.8));
        Location_Num = Math.max(2, Package_Num*2);
        truck = Package_Num * 2;
        train = Package_Num;
        airplane = Package_Num;    
       
    }
    
   
    void generate(){
        int i;
        int j;
        String S;
        
        for(i = 0; i<Problem_Num; i++){
            
            type_p = random.nextInt(6);
            if(type_p == 0){
               type_v = 0;
            }else if(type_p == 1){
                type_v = 1;
            }else if(type_p == 2){
                type_v = 2;
            }else if(type_p == 3){
                type_v = 3;
            }else if(type_p == 4){
                type_v = 0;
            }else{
                type_v = 4;
            }
            
            S = "Problem"+i+"_PDDL.pddl";
            try{
                Pddl = new RandomAccessFile(S, "rw");
                Pddl_Temp = new RandomAccessFile("temp.pddl", "rw");
            }catch(FileNotFoundException e){
                System.out.print("file not found");
            }
            
            Region_City = new int[City_Num];
            Region_Loc = new int[Location_Num];
            Height_Loc = new int[Location_Num];
            Length_Loc = new int[Location_Num];
            Width_Loc = new int[Location_Num];
            Volume_Loc = new int[Location_Num];
            Crane_Num = 0;
            Ramp_Num = 0;
            load = new int[Location_Num];
            train_station = new int[Location_Num];
            hub_train = new boolean[Location_Num];
            num_t = 0;
            airport = new int[Location_Num];
            hub_air = new boolean[Location_Num];
            num_a = 0;
   
            try{
                write_begin(i);
            }catch(IOException e){
                System.out.print("IOException in write_begin method");
            }
            
            try{
                for(j = 0; j<Region_Num; j++){
                    S = "region"+j+" ";
                    Pddl.writeBytes(S);
                }
            
                S = "- region\n";
                Pddl.writeBytes(S);
            }catch(IOException e){
                    System.out.print("IOException in write_region");
            }
            
            
            try{
                S = "          ";
                Pddl.writeBytes(S);
                for(j= 0; j<City_Num; j++){
                    S = "city"+j+" ";
                    Pddl.writeBytes(S);
                    write_city(j);        
	            }
	            S = "- city\n";
                Pddl.writeBytes(S);
             }catch(IOException e){
                System.out.print("IOException in write_city method");
             }
	
	        try{
	            S = "          ";
                Pddl.writeBytes(S);
	            for(j = 0; j< Location_Num; j++){
                    S = "location"+j+" ";
                    Pddl.writeBytes(S);
	                write_loc(j);
	            }
	            S = "- location\n";
                Pddl.writeBytes(S);
             }catch(IOException e){
                    System.out.print("IOException in write_loc method");
             }
             
             try{
	            write_distance();
	         }catch(IOException e){
               System.out.print("IOException in write_distance method");
             }
	        
	        try{
	            S = "          ";
                Pddl.writeBytes(S);
                for(j = 0; j< truck; j++){
                    S = "truck"+j+" ";
                    Pddl.writeBytes(S);
	                write_truck(j);
	            }
	         }catch(IOException e){
                    System.out.print("IOException in write_truck method");
             }
             
             if(num_t != 0){
                try{
	                for(j = 0; j< train; j++){
	                    S = "train"+j+" ";
                        Pddl.writeBytes(S);
	                    write_train(j);
	                }
	            }catch(IOException e){
                    System.out.print("IOException in write_train method");
                }
             }
            
            if(num_a != 0){
	            try{
	                for(j = 0; j< airplane; j++){
	                    S = "airplane"+j+" ";
                        Pddl.writeBytes(S);
	                    write_airplane(j);
	                }
                }catch(IOException e){
                    System.out.println("IOException in write_airplane method");
                }
            }
	        
	        try{
	            S = "- vehicle\n";
                Pddl.writeBytes(S);
            }catch(IOException e){
                System.out.println("writing error");
            }
            
            if(type_p == 1){
	            try{
	                S = "          ";
                    Pddl.writeBytes(S);
                    for(j = 0; j< Location_Num; j++){
                        if(random.nextDouble() > 0.03){
                            S = "crane"+Crane_Num+" ";
                            Pddl.writeBytes(S);
                            write_crane(j);
                        }
	                }
	                if(Crane_Num != 0){
	                    S = "- crane\n";
                        Pddl.writeBytes(S);
                    }
                }catch(IOException e){
                    System.out.print("IOException in write crane");
                }
            }
            
            if(num_a != 0){
                try{
                    S = "          ";
                    Pddl.writeBytes(S);
                    for(j = 0; j < num_a; j++){
                        if(random.nextDouble() > 0.05){
                            int loc = airport[j];
                            S = "plane_ramp"+Ramp_Num+" ";
                            Pddl.writeBytes(S);
                            write_ramp(loc);
                        }
	                }
	                if(Ramp_Num != 0){
	                    S = "- plane-ramp\n";
                        Pddl.writeBytes(S);
                    }
                }catch(IOException e){
                    System.out.print("IOException in write plane ramp");
                }
            }
            
            try{
                S = "          ";
                Pddl.writeBytes(S);
                for(j = 0; j< Package_Num; j++){
                    S = "package"+j+" ";
                    Pddl.writeBytes(S);
                    write_package(j);
	            }
	            S = "- package\n";
                Pddl.writeBytes(S);
             }catch(IOException e){
                    System.out.print("IOException in write_package method");
            }
            
            try{
	            write_road_route();
	        
	            write_rail_route();
	        
	            write_air_route();
	      
	            write_volume_load();
	        
	            write_goal(i);
	        
	            write_Pddl();
	         }catch(IOException e){
                    System.out.print("IOException in finish method");
             }
         }
      
    }
    
    void write_begin(int i) throws IOException{
        String S = "(define (problem problem"+i+")\n";
        Pddl.writeBytes(S);
        S = "   (:domain UM-Translog-2)\n";
        Pddl.writeBytes(S);
        S = "   (:requirements :typing :adl :equality :negative-preconditions :existential-preconditions :universal-preconditions :fluents)\n";
        Pddl.writeBytes(S);
        S = "   (:objects ";
        Pddl.writeBytes(S);
        
        S = "   (:init (pv-compatible regularp regularv)\n";
        Pddl_Temp.writeBytes(S);
        S = "          (pv-compatible bulky flatbed)\n";        
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible liquid tanker)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible cars auto)\n";	    
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible regularp air)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible mail air)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible mail regularv)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (pv-compatible granular hopper)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (rv-compatible air-route airplane)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (rv-compatible rail-route train)\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (rv-compatible road-route truck)\n\n";
	    Pddl_Temp.writeBytes(S);
	}
	 

	void write_city(int j) throws IOException{
       
        int region = random.nextInt(Region_Num);
        
        String S = "          (in-region city"+j+" region"+region+")\n";
        Pddl_Temp.writeBytes(S);
        
        int Height = (int) (Height_Cap_UB * random.nextDouble()) + 7;
	    S = "          (= (local-height city"+j+") "+Height+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int Weight = (int) (Weight_Cap_UB * random.nextDouble()) + 50;            
	    S = "          (= (local-weight city"+j+") "+Weight+")\n";
	    Pddl_Temp.writeBytes(S);
	    Region_City[j] = region;
	}
	
	void write_loc(int j) throws IOException{

      int city = random.nextInt(City_Num);
      
      String S = "          (in-city location"+j+" city"+city+")\n";
	  Pddl_Temp.write(S.getBytes());
	  
	  int Height = (int) (Height_Cap_UB * random.nextDouble()) + 7;
	  Height_Loc[j] = Height;
	  S = "          (= (height-cap-l location"+j+") "+Height+")\n";
	  Pddl_Temp.writeBytes(S);
	  
	  int Length = (int) (Length_Cap_UB * random.nextDouble()) + 10;
	  Length_Loc[j] = Length;
	  S = "          (= (length-cap-l location"+j+") "+Length+")\n";
	  Pddl_Temp.writeBytes(S);
	  
	  int Width = (int) (Width_Cap_UB * random.nextDouble()) + 7;
	  Width_Loc[j] = Width;
	  S = "          (= (width-cap-l location"+j+") "+Width+")\n";
	  Pddl_Temp.writeBytes(S);
	  
	  int Volume_Cap = (int) (Volume_Cap_UB * random.nextDouble()) + 30;
	  Volume_Loc[j] = Volume_Cap;
	  S = "          (= (volume-cap-l location"+j+") "+Volume_Cap+")\n";
	  Pddl_Temp.writeBytes(S);
	  
	  Region_Loc[j] = Region_City[city];
	  
	  int temp = random.nextInt(3);
	  if(temp == 1 || temp == 2){
	    S = "          (tcenter location"+j+")\n";
	    Pddl_Temp.writeBytes(S);	
	    
	    if(temp == 1){
	        S = "          (typel location"+j+" train-station)\n";
	        Pddl_Temp.writeBytes(S);	                    
	    }else{
	        S = "          (typel location"+j+" airport)\n";
	        Pddl_Temp.writeBytes(S);	   
	    }
	    	                    
	    if(random.nextDouble() > 0.05){
	        S = "          (availablel location"+j+")\n";
	        Pddl_Temp.writeBytes(S);
	    }
	                    
	    if(random.nextDouble()>0.3){
	        S = "          (hub location"+j+")\n";
	        Pddl_Temp.writeBytes(S);
	        
	        if(temp == 1){
	            hub_train[num_t] = true;
	        }else{
	            hub_air[num_a] = true;
	        }   
	    }
	   
	    if(temp == 1){
	        train_station[num_t] = j;
	        num_t++;
	    }else{
	        airport[num_a] = j;
	        num_a++;
	    }
	  }
	}
	
    void write_distance() throws IOException{
        
        String S = null;
        
        for(int i = 0; i< Location_Num; i++){
            for(int j = i+1; j < Location_Num; j++){
                int Distance = (int)(Distance_UB * random.nextDouble());
                S = "          (= (distance location"+i+" location"+j+") "+Distance+")\n";
                Pddl_Temp.writeBytes(S);
                S = "          (= (distance location"+j+" location"+i+") "+Distance+")\n";
                Pddl_Temp.writeBytes(S);
	        }
	       
           S = "          (= (distance location"+i+" location"+i+") "+0+")\n";
           Pddl_Temp.writeBytes(S);
	   }
	}         
	
    
    void write_truck(int j) throws IOException{
        
        int location = random.nextInt(Location_Num);
        
        int type = random.nextInt(5);
	    
	    if(random.nextDouble() > 0.05){
	            type = type_v;
	    }
	    
	    int gpm = random.nextInt(Gpm_UB);
	        
	    String S = "          (typev truck"+j+" "+v_type[type]+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    S = "          (typevp truck"+j+" truck)\n";
	    Pddl_Temp.writeBytes(S);
	    
	    S = "          (at-vehicle truck"+j+" location"+location+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int gas = (int) (Gas_UB * random.nextDouble())+ 100;
	    S = "          (= (gas-left truck"+j+") "+gas+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (gpm truck"+j+") "+(gpm+1)+")\n";	  
	    Pddl_Temp.writeBytes(S);
	    
	    int Length = (int)(Length_UB*random.nextDouble()) + 5;
	    int Height = (int)(Height_UB*random.nextDouble()) + 1;
	    int Width = (int)(Width_UB*random.nextDouble()) + 1;
	    
	    if(Length > Length_Loc[location]){
	        Length = Length_Loc[location];
	    }
	    if(Height > Height_Loc[location]){
	        Height = Height_Loc[location];
	    }
	    if(Width > Width_Loc[location]){
	        Width = Width_Loc[location];
	    }
	    S = "          (= (length-v truck"+j+") "+Length+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (height-v truck"+j+") "+Height+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (width-v truck"+j+") "+Width+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int Weight_Cap = (int)(Weight_Cap_UB*random.nextDouble()) + 30;
	    S = "          (= (weight-cap-v truck"+j+") "+Weight_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Volume_Cap = (int)(Volume_Cap_UB*random.nextDouble()) + 10;
	    if(Volume_Cap > Length * Width * Height){
	        Volume_Cap = Length * Width * Height;
	    }
	    if(Volume_Cap < (0.7*Length * Width * Height)){
	        Volume_Cap = (int)(0.7*Length * Width * Height);
	    }
	    S = "          (= (volume-cap-v truck"+j+") "+Volume_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (weight-load-v truck"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (volume-load-v truck"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Weight_v = (int)(WeightV_UB*random.nextDouble()) + 10;
	    S = "          (= (weight-v truck"+j+") "+Weight_v+")\n";
	    Pddl_Temp.writeBytes(S);
	      	         
	    if(random.nextDouble() > 0.05){
	        S = "          (availablev truck"+j+")\n";
	        Pddl_Temp.writeBytes(S);
	    }            
	}
    
   void write_train(int j) throws IOException{
        
       int location = train_station[random.nextInt(num_t)];
       
       
	  int type = random.nextInt(5);
	    
	    if(random.nextDouble() > 0.05){
	            type = type_v;
	    }
	    
	    int gpm = random.nextInt(Gpm_UB);
	    
	    String S = "          (typev train"+j+" "+v_type[type]+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    S = "          (typevp train"+j+" train)\n";
	    Pddl_Temp.writeBytes(S);
	    
	    S = "          (at-vehicle train"+j+" location"+location+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int gas = (int) (Gas_UB * random.nextDouble())+ 50;
	    S = "          (= (gas-left train"+j+") "+gas+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (gpm train"+j+") "+(gpm+1)+")\n";	    
	    Pddl_Temp.writeBytes(S);
	    
	    int Length = (int)(Length_UB*random.nextDouble()) + 5;
	    int Height = (int)(Height_UB*random.nextDouble()) + 1;
	    int Width = (int)(Width_UB*random.nextDouble()) + 1;
	    if(Length > Length_Loc[location]){
	        Length = Length_Loc[location];
	    }
	    if(Height > Height_Loc[location]){
	        Height = Height_Loc[location];
	    }
	    if(Width > Width_Loc[location]){
	        Width = Width_Loc[location];
	    }
	    S = "          (= (length-v train"+j+") "+Length+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (height-v train"+j+") "+Height+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (width-v train"+j+") "+Width+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int Weight_Cap = (int)(Weight_Cap_UB*random.nextDouble()) + 30;
	    S = "          (= (weight-cap-v train"+j+") "+Weight_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Volume_Cap = (int)(Volume_Cap_UB*random.nextDouble()) + 10;
	    if(Volume_Cap > Length * Width * Height){
	        Volume_Cap = Length * Width * Height;
	    }
	    if(Volume_Cap < (0.7*Length * Width * Height)){
	        Volume_Cap = (int)(0.7*Length * Width * Height);
	    }
	    S = "          (= (volume-cap-v train"+j+") "+Volume_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (weight-load-v train"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (volume-load-v train"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Weight_v = (int)(WeightV_UB*random.nextDouble()) + 10;
	    S = "          (= (weight-v train"+j+") "+Weight_v+")\n";
	    Pddl_Temp.writeBytes(S);
	      	         
	    if(random.nextDouble() > 0.05){
	        S = "          (availablev train"+j+")\n";
	        Pddl_Temp.writeBytes(S);
	    }            
	}
	
	void write_airplane(int j) throws IOException{
        
       int location = airport[random.nextInt(num_a)];
            
	   int gpm = random.nextInt(Gpm_UB);
	        
	   String S = "          (typev airplane"+j+" air)\n";
	   Pddl_Temp.writeBytes(S);
	   
	   S = "          (typevp airplane"+j+" airplane)\n";
	   Pddl_Temp.writeBytes(S);
	   
	    S = "          (at-vehicle airplane"+j+" location"+location+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int gas = (int) (Gas_UB * random.nextDouble())+ 50;
	    S = "          (= (gas-left airplane"+j+") "+gas+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (gpm airplane"+j+") "+(gpm+1)+")\n";	     
	    Pddl_Temp.writeBytes(S);
	    
	    int Length = (int)(Length_UB*random.nextDouble()) + 5;
	    int Height = (int)(Height_UB*random.nextDouble()) + 1;
	    int Width = (int)(Width_UB*random.nextDouble()) + 1;
	    if(Length > Length_Loc[location]){
	        Length = Length_Loc[location];
	    }
	    if(Height > Height_Loc[location]){
	        Height = Height_Loc[location];
	    }
	    if(Width > Width_Loc[location]){
	        Width = Width_Loc[location];
	    }
	    S = "          (= (length-v airplane"+j+") "+Length+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (height-v airplane"+j+") "+Height+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (width-v airplane"+j+") "+Width+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    int Weight_Cap = (int)(Weight_Cap_UB*random.nextDouble()) + 30;
	    S = "          (= (weight-cap-v airplane"+j+") "+Weight_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Volume_Cap = (int)(Volume_Cap_UB*random.nextDouble()) + 10;
	    
	    if(Volume_Cap > Length * Width * Height){
	        Volume_Cap = Length * Width * Height;
	    }
	    if(Volume_Cap < (0.7*Length * Width * Height)){
	        Volume_Cap = (int)(0.7*Length * Width * Height);
	    }
	    
	    S = "          (= (volume-cap-v airplane"+j+") "+Volume_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (weight-load-v airplane"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (= (volume-load-v airplane"+j+") "+0+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Weight_v = (int)(WeightV_UB*random.nextDouble()) + 10;
	    S = "          (= (weight-v airplane"+j+") "+Weight_v+")\n";
	    Pddl_Temp.writeBytes(S);
	      	         
	    if(random.nextDouble() > 0.05){
	        S = "          (availablev airplane"+j+")\n";
	        Pddl_Temp.writeBytes(S);
	    }            
	    
	}
	
    void write_crane(int i) throws IOException{
        
        String S = "          (at-equipment crane"+Crane_Num+" location"+i+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (empty crane"+Crane_Num+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Weight_Cap = (int)(Weight_Cap_UB*random.nextDouble()) + 10;
	    S = "          (= (weight-cap-c crane"+Crane_Num+") "+Weight_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Volume_Cap = (int)(Volume_Cap_UB*random.nextDouble()) + 10;
	    S = "          (= (volume-cap-c crane"+Crane_Num+") "+Volume_Cap+")\n";
	    Pddl_Temp.writeBytes(S);
	    
	    Crane_Num ++;
	}
	
	 
	void write_ramp(int i) throws IOException{
	    
	    String S = "          (at-equipment plane_ramp"+Ramp_Num+" location"+i+")\n";
	    Pddl_Temp.writeBytes(S);	   
	    Ramp_Num ++;
	}
	
	void write_package(int j) throws IOException{
        
        int loc = random.nextInt(Location_Num);
        
        if(Package_Num == 1){
            package_loc = loc;
        }
           
        String S = "          (typep package"+j+" "+p_type[type_p]+")\n";
	    Pddl_Temp.writeBytes(S);
	    S = "          (at-packagel package"+j+" location"+loc+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Volume = (int)(VolumeP_UB * random.nextDouble()) + 5;
	    
	    if(Volume > Volume_Loc[loc]){
	        Volume = (int) (0.3*Volume_Loc[loc]);
	    }
	    
	    if((load[loc] + Volume) > Volume_Loc[loc]){
	        Volume = Volume_Loc[loc] - load[loc];
	    }
	    load[loc] += Volume;
	    S = "          (= (volume-p package"+j+") "+Volume+")\n";
	    Pddl_Temp.writeBytes(S);
	    int Weight = (int)(WeightP_UB * random.nextDouble()) + 5;
	    S = "          (= (weight-p package"+j+") "+Weight+")\n";
	    Pddl_Temp.writeBytes(S);
	          
	}
	
	void write_road_route() throws IOException{
        
        String S = null;
        
        int n = 0;
                   
        for(int i = 0; i< City_Num; i++){
	        for(int j = 0; j< City_Num; j++){
	            if((j != i) && (random.nextDouble() > 0.03)){
	                
	                if(n == 0){
	                    S = "          ";
	                    Pddl.writeBytes(S);
	                }
	                
	                S = "road_route"+n+" ";
	                Pddl.writeBytes(S);
	                
	                
	                S = "          (connect-city road_route"+n+" road-route city" + i + " city"+j+")\n";
	                Pddl_Temp.writeBytes(S);
	                
	                int Height_Cap = (int)(Height_Cap_UB * random.nextDouble()) + 5;
	                S = "          (= (height-cap-r road_route"+n+") "+Height_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	                int Weight_Cap = (int)(Weight_Cap_UB * random.nextDouble()) + 50;
	                S = "          (= (weight-cap-r road_route"+n+") "+Weight_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	              
	                if(random.nextDouble() >0.05){
	                    S = "          (availabler road_route"+n+")\n";
	                    Pddl_Temp.writeBytes(S);
	                }
	                
	                n++;
	            }
	        }
	    }
	    
	    if(n >= 1){
	        S = "- route\n";
            Pddl.writeBytes(S);
        }
	}
	
	void write_rail_route() throws IOException{
        
        if(num_t == 0){
            return;
        }
        String S = null;
        
        Region_Serve_T = new int[Region_Num][num_t];
        
        int n = 0;
        for(int i = 0; i< num_t; i++){
	        for(int j = 0; j< num_t; j++){
	            if(j != i && (random.nextDouble() > 0.05)){
	                int loc1 = train_station[i];
	                int loc2 = train_station[j];
	                int reg1 = Region_Loc[loc1];
	                int reg2 = Region_Loc[loc2];
	                
	                if(n == 0){
	                    S = "          ";
	                    Pddl.writeBytes(S);
	                }
	                
	                S = "rail_route"+n+" ";
	                Pddl.writeBytes(S);
	                
	                S = "          (connect-loc rail_route"+n+" rail-route location"+loc1+" location"+loc2+")\n";
	                Pddl_Temp.writeBytes(S);
	                int Height_Cap = (int)(Height_Cap_UB * random.nextDouble()) + 5;
	                S = "          (= (height-cap-r rail_route"+n+") "+Height_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	                int Weight_Cap = (int)(Weight_Cap_UB * random.nextDouble()) + 50;
	                S = "          (= (weight-cap-r rail_route"+n+") "+Weight_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	               
	                if(hub_train[i]){
	                    if(Region_Serve_T[reg2][i] != 1){
	                        Region_Serve_T[reg2][i] = 1;
	                        S = "          (serves location"+loc1+" region"+reg2+")\n";  
	                        Pddl_Temp.writeBytes(S);
	                    }
	                }
	                
	                if(hub_train[j]){
	                    if(Region_Serve_T[reg1][j] != 1){
	                        Region_Serve_T[reg1][j] = 1;
	                        S = "          (serves location"+loc2+" region"+reg1+")\n";  
	                        Pddl_Temp.writeBytes(S);
	                    }
	                }
	                
	                    
	                if(random.nextDouble() >0.05){
	                    S = "          (availabler rail_route"+n+")\n";         
	                    Pddl_Temp.writeBytes(S);
	                }
	                n++;
	            }
	        }
	    }
	    
	    if(n >= 1){
	        S = "- route\n";
            Pddl.writeBytes(S);
        }
	}
	
	void write_air_route() throws IOException{
        
        String S = null;
        
        Region_Serve_A = new int[Region_Num][num_a];
        
        int n = 0;
        for(int i = 0; i< num_a; i++){
	        for(int j = 0; j< num_a; j++){
	            if(j != i&& (random.nextDouble() > 0.05)){
	                int loc1 = airport[i];
	                int loc2 = airport[j];
	                int reg1 = Region_Loc[loc1];
	                int reg2 = Region_Loc[loc2];
	                
	                if(n == 0){
	                    S = "          ";
	                    Pddl.writeBytes(S);
	                }
	                
	                S = "air_route"+n+" ";
	                Pddl.writeBytes(S);
	                
	              
	                S = "          (connect-loc air_route"+n+" air-route location"+loc1+" location"+loc2+")\n";
	                Pddl_Temp.writeBytes(S);
	                int Height_Cap = (int)(Height_Cap_UB * random.nextDouble()) + 5;
	                S = "          (= (height-cap-r air_route"+n+") "+Height_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	                int Weight_Cap = (int)(Weight_Cap_UB * random.nextDouble()) + 50;
	                S = "          (= (weight-cap-r air_route"+n+") "+Weight_Cap+")\n";
	                Pddl_Temp.writeBytes(S);
	                
	                if(hub_air[i]){
	                    if(Region_Serve_A[reg2][i] != 1){
	                        Region_Serve_A[reg2][i] = 1;
	                        S = "          (serves location"+loc1+" region"+reg2+")\n";
	                        Pddl_Temp.writeBytes(S);
	                    }
	                }
	                
	                if(hub_air[j]){
	                    if(Region_Serve_A[reg1][j] != 1){
	                        Region_Serve_A[reg1][j] = 1;
	                        S = "          (serves location"+loc2+" region"+reg1+")\n";   
	                        Pddl_Temp.writeBytes(S);
	                    }
	                }
	                
	                if(random.nextDouble() >0.05){
	                    S = "          (availabler air_route"+n+")\n";           
	                    Pddl_Temp.writeBytes(S);
	                }
	                n++;
	            }
	        }
	    }
	    
	    if(n >= 1){
	        S = "- route\n";
            Pddl.writeBytes(S);
        }
	}
	
	void write_volume_load() throws IOException{
        String S = null;
        for(int i = 0; i< Location_Num; i++){
            int a = load[i];
            S = "          (= (volume-load-l location"+i+") "+a+")\n";
            Pddl_Temp.writeBytes(S);
        }
        S = "   )\n\n";
        Pddl_Temp.writeBytes(S);
    }
    
    void write_goal(int i) throws IOException{
        String S = "   (:goal (and ";
        Pddl_Temp.writeBytes(S);
        
        for(int j = 0; j< Package_Num; j++){ 
            int loc = 0;
            
            if(Package_Num == 1 && random.nextDouble() > 0.05){
                if(package_loc == 0){
                    loc = 1;
                }else{
                    loc = 0;
                }
            }else if(Package_Num == 1){
                loc = package_loc;
            }
            
            if(Package_Num != 1){
                loc = random.nextInt(Location_Num);
            }
            if(j == 0){
                S = "(delivered package"+j+" location"+loc+")\n";
            }else{
                S = "               (delivered package"+j+" location"+loc+")\n";
            }
            Pddl_Temp.writeBytes(S);
        }
	    S = "               (clear)))\n\n)";
	    Pddl_Temp.writeBytes(S);
    }
    
   
    void write_Pddl() throws IOException{
        Pddl.writeBytes("   )\n");
        Pddl_Temp.seek(0);
        long length = Pddl_Temp.length();
        byte[] aa = new byte[(int)length];
        
        Pddl_Temp.read(aa);
        Pddl_Temp.close();
        
        File bb = new File("temp.pddl");
        bb.delete();
        
        Pddl.write(aa);
        Pddl.close();
    }
        
   
}
    
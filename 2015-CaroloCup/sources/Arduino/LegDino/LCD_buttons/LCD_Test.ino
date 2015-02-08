// Paint example specifically for the TFTLCD Arduino shield.
// If using the breakout board, use the tftpaint.pde sketch instead!

#include <Adafruit_GFX.h>    // Core graphics library
#include <Adafruit_TFTLCD.h> // Hardware-specific library
#include <TouchScreen.h>

#if defined(__SAM3X8E__)
    #undef __FlashStringHelper::F(string_literal)
    #define F(string_literal) string_literal
#endif

#ifndef USE_ADAFRUIT_SHIELD_PINOUT 
 #error "This sketch is intended for use with the TFT LCD Shield. Make sure that USE_ADAFRUIT_SHIELD_PINOUT is #defined in the Adafruit_TFTLCD.h library file."
#endif

// These are the pins for the shield!
#define YP A1  // must be an analog pin, use "An" notation!
#define XM A2  // must be an analog pin, use "An" notation!
#define YM 7   // can be a digital pin
#define XP 6   // can be a digital pin

#ifdef __SAM3X8E__
  #define TS_MINX 125
  #define TS_MINY 170
  #define TS_MAXX 880
  #define TS_MAXY 940
#else
  #define TS_MINX  150
  #define TS_MINY  120
  #define TS_MAXX  920
  #define TS_MAXY  940
#endif

// For better pressure precision, we need to know the resistance
// between X+ and X- Use any multimeter to read it
// For the one we're using, its 300 ohms across the X plate
TouchScreen ts = TouchScreen(XP, YP, XM, YM, 300);

#define LCD_CS A3
#define LCD_CD A2
#define LCD_WR A1
#define LCD_RD A0

// Assign human-readable names to some common 16-bit color values:
#define	BLACK   0x0000
#define	BLUE    0x001F
#define	RED     0xF800
#define	GREEN   0x07E0
#define CYAN    0x07FF
#define MAGENTA 0xF81F
#define YELLOW  0xFFE0
#define WHITE   0xFFFF


Adafruit_TFTLCD tft;

#define BOXSIZE   120
#define CLICKBOX   60
#define PENRADIUS  8
int currentkey=0;

void setup(void) {
  Serial.begin(9600);
  //tft.reset();

  uint16_t identifier = tft.readID();

  if(identifier == 0x9325) {
    Serial.println(F("Found ILI9325 LCD driver"));
  } else if(identifier == 0x9328) {
    Serial.println(F("Found ILI9328 LCD driver"));
  } else if(identifier == 0x7575) {
    Serial.println(F("Found HX8347G LCD driver"));
  } else {
    Serial.print(F("Unknown LCD driver chip: "));
    Serial.println(identifier, HEX);
    return;
  }

  tft.begin(identifier);

  tft.fillScreen(BLACK);

  tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
  tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
  tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
  tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
  tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
 
  pinMode(13, OUTPUT);
}

#define MINPRESSURE 10
#define MAXPRESSURE 1000

void loop()
{
  digitalWrite(13, HIGH);
  Point p = ts.getPoint();
  digitalWrite(13, LOW);

  // if sharing pins, you'll need to fix the directions of the touchscreen pins
  //pinMode(XP, OUTPUT);
  pinMode(XM, OUTPUT);
  pinMode(YP, OUTPUT);
  //pinMode(YM, OUTPUT);

  // we have some minimum pressure we consider 'valid'
  // pressure of 0 means no pressing!

  if (p.z > MINPRESSURE && p.z < MAXPRESSURE) {
    // scale from 0->1023 to tft.width
    p.x = map(p.x, TS_MINX, TS_MAXX, tft.width(), 0);
    p.y = map(p.y, TS_MINY, TS_MAXY, tft.height(), 0);
    //Serial.println(tft.width());
    //Serial.println(tft.height());
    if (p.y < BOXSIZE) {

       if (p.x < BOXSIZE) { 
         currentkey=0;
         Serial.println(currentkey);
	 Serial.flush();
         tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
         tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
         tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
         tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
         tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
         tft.fillRect(30, 30, CLICKBOX, CLICKBOX, WHITE);
        // tft.drawRect(0, 40, BOXSIZE, BOXSIZE, WHITE);
       } else if (p.x < BOXSIZE*2) {
         currentkey=1;
         tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
         tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
         tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
         tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
         tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
         tft.fillRect(BOXSIZE+30, 30, CLICKBOX, CLICKBOX, WHITE);
         //Serial.println("MODE1");
         //tft.drawRect(BOXSIZE, 40, BOXSIZE, BOXSIZE, WHITE);
       }
    }
        if (p.y > BOXSIZE && p.y < BOXSIZE*2) {
      // oldcolor = currentcolor;

       if (p.x < BOXSIZE) {
         currentkey=2;
         tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
         tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
         tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
         tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
         tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
         tft.fillRect(30, BOXSIZE+30, CLICKBOX, CLICKBOX, WHITE);
        // Serial.println("MODE2");
         //tft.drawRect(0,BOXSIZE+40, BOXSIZE, BOXSIZE, WHITE);
       } else if (p.x < BOXSIZE*2) {
         currentkey = 3;
         tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
         tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
         tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
         tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
         tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
         tft.fillRect(BOXSIZE+30, BOXSIZE+30, CLICKBOX, CLICKBOX, WHITE);
         //Serial.println("MODE3");
         //tft.drawRect(BOXSIZE, BOXSIZE+40, BOXSIZE, BOXSIZE, WHITE);
       }
        }
       if (p.y > BOXSIZE*2 && p.y < 320) {
       //  Serial.println("MODE3");
      // oldcolor = currentcolor;
      if(currentkey != 0){
         Serial.println(currentkey);
	 Serial.flush();
         tft.fillRect(0, 0, BOXSIZE, BOXSIZE, RED);
         tft.fillRect(BOXSIZE, 0, BOXSIZE, BOXSIZE, YELLOW);
         tft.fillRect(0, BOXSIZE, BOXSIZE, BOXSIZE, GREEN);
         tft.fillRect(BOXSIZE, BOXSIZE, BOXSIZE, BOXSIZE, CYAN);
         tft.fillRect(0, BOXSIZE*2, 240, 320, BLUE);
         tft.fillRect(90, BOXSIZE*2+20, 60, 40, WHITE);
         //tft.drawRect(0,BOXSIZE+40, BOXSIZE, BOXSIZE, WHITE);
      
       }
       
    }

  }
}


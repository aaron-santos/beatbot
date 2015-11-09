/**
 * A Processing implementation of izhikevich spiking neural network.
 * http://www.izhikevich.org/publications/spikes.pdf
 * By Aaron Santos
 */
import java.util.Set;
import java.util.HashSet;
import org.dishevelled.processing.executor.Executor;
import java.util.concurrent.TimeUnit;

// Size of cells
int cellSize = 5;

int numExcitatory = 800;
int numInhibitory = 200;
int totalNeurons = numExcitatory + numInhibitory;
int cellsWidth = (int) Math.ceil(Math.sqrt(totalNeurons));
int cellsHeight = cellsWidth;

float[] randExcitatory = new float[numExcitatory];
float[] randInhibitory = new float[numInhibitory];

float[] as = new float[totalNeurons];
float[] bs = new float[totalNeurons];
float[] cs = new float[totalNeurons];
float[] ds = new float[totalNeurons];

float[][] Ss = new float[totalNeurons][totalNeurons];
float[] vs = new float[totalNeurons];
float[] us = new float[totalNeurons];
float[] Is = new float[totalNeurons];

Set<Integer> firings = new HashSet<Integer>();
int width = cellsWidth * cellSize;
int height = cellsHeight * cellSize;

Executor executor;
PFont font;
volatile long iterations;

void setup() {
  size (160, 160);
  
  frameRate(60);

  // This stroke will draw the background grid
  stroke(48);

  noSmooth();

  // Initialization of arrays
  for (int idx=0; idx < totalNeurons; idx++) {
    if (idx < numExcitatory) {
      randExcitatory[idx] = random(1);
      as[idx] = 0.02;
      bs[idx] = 0.2;
      cs[idx] = -65 + 15 * randExcitatory[idx] * randExcitatory[idx];
      ds[idx] = 8 - 6 * randExcitatory[idx] * randExcitatory[idx];
      for (int colIdx = 0; colIdx < totalNeurons; colIdx++) {
        Ss[idx][colIdx] = 0.5 * random(1);
      }
      vs[idx] = -65;
      us[idx] = vs[idx] + vs[idx];
    } else {
      int inIdx = idx - numExcitatory;
      randInhibitory[inIdx] = random(1);
      as[idx] = 0.02 + 0.08 * randInhibitory[inIdx];
      bs[idx] = 0.25 - 0.08 * randInhibitory[inIdx];
      cs[idx] = -65;
      ds[idx] = 2;
      for (int colIdx = 0; colIdx < totalNeurons; colIdx++) {
        Ss[idx][colIdx] = -random(1);
      }
      vs[idx] = -65;
      us[idx] = vs[idx] + vs[idx];
    }
  }
  background(0); // Fill in black in case cells don't cover all the windows
  
  font = createFont("Arial Bold", 12);
  executor = new Executor(this, 1);
  //executor.repeat("iteration", 0, 1, TimeUnit.MILLISECONDS);
  executor.repeat("iteration", 0, 606, TimeUnit.MICROSECONDS);
  iterations = millis();
}


void draw() {

  //Draw grid
  for (int idx=0; idx < totalNeurons; idx++) {
    float v = vs[idx];
    int x = idx % cellsWidth;
    int y = idx / cellsHeight;
    fill(color(v, 128, 128));
    rect (x*cellSize, y*cellSize, cellSize, cellSize);
  }
  // white float frameRate
  textFont(font,18);
  fill(255);
  text((float) iterations / millis(), 10, 20);
}

void iteration() { // When the clock ticks
  firings.clear();
  
  for (int idx=0; idx < totalNeurons; idx++) {
    if (idx < numExcitatory) {
      Is[idx] = random(5);
    } else {
      Is[idx] = random(2);
    }
    if (vs[idx] >= 30) {
      firings.add(idx);
    }
  }
  
  for (int firedIdx : firings) {
    vs[firedIdx] = cs[firedIdx];
    us[firedIdx] = us[firedIdx] + ds[firedIdx];
    for (int idx=0; idx < totalNeurons; idx++) {
      Is[idx] += Ss[firedIdx][idx];
    }
  }
  
  for (int idx=0; idx < totalNeurons; idx++) {
    float v = vs[idx];
    float u = us[idx];
    float I = Is[idx];
    vs[idx] += 0.5 * ((0.04 * v * v) + (5 * v) + 140 - u + I);
    v = vs[idx];
    vs[idx] += 0.5 * ((0.04 * v * v) + (5 * v) + 140 - u + I);
    us[idx] += as[idx] * (bs[idx] * vs[idx] - us[idx]);
  }
  iterations++;
} // End of function
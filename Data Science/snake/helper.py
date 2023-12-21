import matplotlib.pyplot as plt
from IPython import display

plt.ion()

def plot(scores, mean_scores): #plot stuff
    display.clear_output(wait=True)
    display.display(plt.gcf())
    plt.clf()
    plt.title('Training...') #title of graph
    plt.xlabel('Number of Games') #x axis label
    plt.ylabel('Score') #y axis label
    plt.plot(scores) #plotting list of scores
    plt.plot(mean_scores) #plotting list of mean scores
    plt.ylim(ymin=0)
    plt.text(len(scores)-1, scores[-1], str(scores[-1]))
    plt.text(len(mean_scores)-1, mean_scores[-1], str(mean_scores[-1]))
    plt.show(block=False)
    plt.pause(.1)

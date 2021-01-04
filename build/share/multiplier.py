#!/usr/bin/env python

"""
multiplier.py - Interactively transform a file's numbers via multiplication
"""

import getopt       # for parsing input arguments
import os           # For filesystem stuff
import sys          # System utilities
import re           # for finding numbers


######################################################################
## Constants

WINDOWSIZE = 5      # Number of context lines to show: ASSUMED TO BE ODD
# RegExp for Integer, from https://stackoverflow.com/a/48041305
INTEGER_RE = re.compile('[-+]?([1-9]\d*|0)')
MULTIPLIER = 8


######################################################################
## Procedures

def fileExists(fname):
    """
    fileExists(fname): return True if file fname exists, False otherwise
    """
    return os.access(fname,os.F_OK)


def displayContext(lines,line,startColumn,endColumn):
    """
    displayContext(lines,line,startColumn,endColumn):
        display text from a file with start-end on some line highlighted.
        lines : listof String, list of file lines
        line : 0-based index of the line to display
        [startColumn,endColumn) is the half-open range of characters to 
          highlight
    """

    # Calculate lines window:
    # default
    startLine = line-WINDOWSIZE/2
    endLine = line+1+WINDOWSIZE/2

    # prune the window to handle the boundaries as needed
    if startLine < 0:
        startLine=0

    if endLine > len(lines):
        endLine = len(lines)

    # print the preLines and the line
    for i in range(startLine,line+1):
        sys.stdout.write(lines[i])

    # print the highlight line
    sys.stdout.write(' ' * startColumn)
    sys.stdout.write('^' * (endColumn-startColumn))
    sys.stdout.write('\n')
    
    # print the postLines
    for i in range(line+1,endLine):
        sys.stdout.write(lines[i])
    

def multiplyString(s,n):
    """
    multiplyString(s,n): produce the string representation of the given
    number, multiplied by n
    """
    return str(int(s)*n)

def processNumber(lines,lineIdx,start,end):
    """
    processNumber(lines,lineIdx,start,end): Query about update and perform if
      desired.
      EFFECT: modifies lines[lineIdx]
    """

    # Query the user until they say a right thing
    displayContext(lines,lineIdx,start,end)
    while True:
        sys.stdout.write('Update? ([Y]es/[N]o/[A]bort): ')
        input = sys.stdin.readline()
        status = re.match('^[YyNnAa]\n$',input)
        if status is None:
            continue
        else:
            result = input[0].upper()
            break

    # Process user response
    if result == 'A':
        return 'abort'
    elif result == 'N':
        return end # continue after current number
    elif result == 'Y':  # multiply!
        line = lines[lineIdx]
        newNum = multiplyString(line[start:end],MULTIPLIER)
        newCol = start + len(newNum)
        lines[lineIdx] = line[0:start] + newNum + line[end:]
        return newCol
    else:
        assert(False)

    
def multiplyLine(lines,lineIdx):
    """
    multiplyLine(lines,lineIdx): multiply the numbers in a given line
      EFFECT: modifies lines[lineIdx]
    """

    colIdx = 0 # start in first column
    # Two-step while loop (predicate follows immediately)
    while True:
        searchResult = INTEGER_RE.search(lines[lineIdx],colIdx)
        if searchResult is None:
            break

        # Found a match: query the user
        start,end = searchResult.span()
        status = processNumber(lines,lineIdx,start,end)
        if status == 'abort':
            return 'abort'
        elif type(status) is int:
            colIdx = status
        else:
            assert(False)

    return 'continue'


def multiplyLines(lines):
    """
    multiplyLines(lines): multiply the numbers in the given lines.
      EFFECT: modifies lines
      RETURNS:
        'abort' - user aborted; 
        'done' - traversal complete
    """
    # Walk each line of the file, updating line in-place
    for lineIdx in range(len(lines)):
        status = multiplyLine(lines,lineIdx)
        if status == 'continue':
            pass
        elif status == 'abort':
            return 'abort'
        else:
            assert(False)
            
    return 'done'
    

def multiplyFile(inFileName,outFileName):
    """
    multiplierFile(inFileName,outFileName):
        interactively multiply numbers in a file
        EFFECT: reads from inFileName; writes to outFileName
    """
    
    # Open and sanity-check the input exam sheet data    
    with open(inFileName,'rU') as f:
        lines = f.readlines()

        # Interactively multiply numbers in lines
        status = multiplyLines(lines)
        if status == 'done':
            pass
        elif status == 'abort':
            sys.stdout.write('Aborting...\n')
            sys.exit(0)
        else:
            assert(False)


    # Write output file to disk
    sys.stdout.write('Writing updates to %s...\n' % outFileName)

    with open(outFileName,'w') as f:
        for line in lines:
            f.write(line)

    sys.stdout.write('Done...\n')
    sys.exit(0)

                             
    


def main():
    """
    main(): process command-line arguments, then multiply some lines!
    """
    if len(sys.argv) != 3:
        usage()

    inFile = sys.argv[1]
    outFile = sys.argv[2]

    # Make surer inFile exists and outFile doesn't
    errorDetected = False
    if not fileExists(inFile):
        sys.stderr.write('ERROR: input file %s does not exist.\n' % inFile)
        errorDetected = True

    if fileExists(outFile):
        sys.stderr.write('ERROR: output file %s aleady exists.\n' % outFile)
        errorDetected = True

    if inFile == outFile:
        sys.stderr.write('ERROR: input and output files must differ.\n')
        errorDetected = Trrue
        
    if errorDetected:
        usage()
        
    multiplyFile(inFile,outFile)
    


def usage():
    """
    usage():  Display a message about how to use this program
    """

    sys.stderr.write(\
"""
USAGE: multiplier.py <infile> <outfile>
multiplier will not overwrite an existing file. You'll have to 
deal with that youself, sorry!
"""
)
    sys.exit(1)


# Standard-ish command-line bootstrap code

if __name__ == '__main__':
    main()


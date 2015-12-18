/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Fall 2015                                *
 *  Author: Ulrich Kremer                    *
 *  Student Version                          *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

int findNumOfRegisters(Instruction*);
void checkIfRegIsUsed(Instruction*, int*);
void deleteOccurrencesOfRegister(Instruction*, int, int);

int main()
{
	Instruction *head;
	int* regs;
	int numRegs, i;
	numRegs = 0;
	regs = NULL;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	/* YOUR CODE GOES HERE */

	// Prereq: Know how many registers there are
	numRegs = findNumOfRegisters(head);	

	// Build array with length numRegs
	// Start with all -1, if unused register
	// regs[0] corresponds to r1
	// regs[i] corresponds to r(i+1)
	regs = (int*) malloc(sizeof(int) * numRegs);
	for (i = 0; i < numRegs; i++)
		regs[i] = -1;

	// Find all outputs

	// First: make an array of all regs that contribute to outputs
	// regs[i] = -1 : Unused, can delete
	// regs[i] = 1 : Used, keep
	checkIfRegIsUsed(head, regs);

	// Second: delete all unused regs
	for (i = 0; i < numRegs; i++)
		if (regs[i] != 1)
			deleteOccurrencesOfRegister(head, i+1, regs[i]);

	// It should noe be optimized

	if (head) 
		PrintInstructionList(stdout, head);
	
	return EXIT_SUCCESS;
}

int findNumOfRegisters(Instruction *head) {
	int numRegs;
	Instruction *ptr;

	numRegs = 0;
	ptr = head;
	ptr = ptr->next;
	// skip the first line, because no one cares

	while(ptr != NULL) {
		switch(ptr->opcode) {
		case LOADI:
			if (numRegs < ptr->field2)
				numRegs = ptr->field2;
			break;	
		case STOREAI:
			if (numRegs < ptr->field1)
				numRegs = ptr->field1;
			break;
		case ADD:
		case SUB:
		case MUL:
		case DIV:
		case LOADAI:
			if (numRegs < ptr->field3)
				numRegs = ptr->field3;
			break;
		case OUTPUTAI:			
			break;
		default:
			ERROR("Illegal instructions\n");
		}
		
		ptr = ptr->next;
	}


	return numRegs;
}

void checkIfRegIsUsed(Instruction *head, int *regs) {
	Instruction *ptr;
	int numOuts, outIterator, isCorrectOut;
	// we don't care about the 0 register, but keep an offset so it's less confusing
	ptr = head->next;
	numOuts = 0;
	// Flag:
	// -1 : Unused
	// 1 : Used

	// Iterate until ptr hits the first OUTPUTAI
	// go backwards and find all things that link to OUTPUTAI
	// If it does link with the register, then mark flag as 1 and return 1
	// If nothing is linked, go to next OUTPUTAI and go backwards to find anything linked to it
	
	// On the way backwards, we can ignore OUTPUTAI, and LOADI

	// Find how many outputs there are
	while (ptr != NULL) {
		if (ptr->opcode == OUTPUTAI)
			numOuts++;
		ptr = ptr->next;
	}

	// We now know how many outputs there are
	ptr = head->next;
	for (outIterator = 0; outIterator < numOuts; outIterator++) {
		isCorrectOut = 0;
		while(ptr != NULL) {
			if (ptr->opcode == OUTPUTAI && isCorrectOut == outIterator) {
				break;
			}
			else if (ptr->next->opcode == OUTPUTAI && isCorrectOut != outIterator) {
				ptr = ptr->next->next;
				isCorrectOut++;
			}
			else {
				ptr = ptr->next;
			}
		}

		regs[(ptr->field2)/4] = 2;
		// ptr is now at OUTPUTAI
		// Go backwards and check if reg is used
		while(ptr != NULL) {
			switch(ptr->opcode) {
			case STOREAI:
				if (regs[(ptr->field3)/4] == 2) { //if the resulting register is critical
					regs[(ptr->field3)/4] = 3;
					regs[ptr->field1 - 1] = 1;
				}
				if (regs[(ptr->field3)/4] == 1) {
					regs[ptr->field1 - 1] = 1;
				}
				break;
			case ADD:
			case SUB:
			case MUL:
			case DIV:
				if (regs[ptr->field3 - 1] == 1) { // if the resulting register is critical
					regs[ptr->field1 - 1] = 1;
					regs[ptr->field2 - 1] = 1;
				}
				break;
			case LOADAI:
				if (regs[ptr->field3 - 1] == 1) { // if the resulting register is critical
					if (regs[(ptr->field2)/4] != 1) {
						regs[(ptr->field2)/4] = 2;
					}
				}
				break;
			case LOADI: // Not relevant, so skip
			case OUTPUTAI:
				break;
			default:
				ERROR("Illegal instructions\n");		
			}
			
			ptr = ptr->prev;
		}
	}

}

void deleteOccurrencesOfRegister(Instruction *head, int reg, int criticalFlag) {
	Instruction *ptr, *temp;
	ptr = head;
	ptr = ptr->next; //skip the first line

	while(ptr != NULL) {
		switch(ptr->opcode) {
		case ADD:
		case SUB:
		case MUL:
		case DIV:
			if (ptr->field1 == reg || ptr->field2 == reg || ptr->field3 == reg) {
				temp = ptr;
				ptr = ptr->prev;
				ptr->next = ptr->next->next;
				ptr->next->prev = ptr;
				free(temp);
			}
			break;
		case STOREAI:
			if (ptr->field1 == reg || (ptr->field3 + 4)/4 == reg) {
				if (ptr->field1 != reg)
					if ((ptr->field3 + 4)/4 == reg && (criticalFlag == 2 || criticalFlag == 3))
						break;
				temp = ptr;
				ptr = ptr->prev;
				ptr->next = ptr->next->next;
				ptr->next->prev = ptr;
				free(temp);
			}
			break;
		case LOADI:
			if (ptr->field2 == reg) {
				temp = ptr;
				ptr = ptr->prev;
				ptr->next = ptr->next->next;
				ptr->next->prev = ptr;
				free(temp);
			}
			break;
		case LOADAI:
			if ((ptr->field2 + 4)/4 == reg || ptr->field3 == reg) {
				if (ptr->field3 != reg)
					if ((ptr->field2 + 4)/4 == reg && (criticalFlag == 2 || criticalFlag == 3)) 						break;
				temp = ptr;
				ptr = ptr->prev;
				ptr->next = ptr->next->next;
				ptr->next->prev = ptr;
				free(temp);
			}
			break;
		case OUTPUTAI:
			break;
		default:
			ERROR("Illegal instructions\n");
		}
		
		ptr = ptr->next;
	}
}

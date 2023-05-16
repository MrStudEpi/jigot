##
## EPITECH PROJECT, 2022
## wolfram
## File description:
## Makefile
##

BINARY_PATH 	:=	$(shell stack path --local-install-root)
NAME 			= 	glados

all:
	stack build
	cp $(BINARY_PATH)/bin/$(NAME)-exe ./$(NAME)

test:
	stack test --coverage

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re test
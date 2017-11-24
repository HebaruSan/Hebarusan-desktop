/*
 *  Show a DOS menu, let the user select with arrows and run with enter.
 *  Also supports typing substrings.
 *
 *  Edit the menu array to customize.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <dos.h>
#include <dir.h>
#include <conio.h>

struct {
	char *friendly, *drive, *dir, *run;
} menu[] = {
	{"Borland C",                    "c", "\\src",  "\\borlandc\\bin\\bc", },
	{"WordPerfect 5.1",              "c", "\\",     "\\wp51\\wp.exe",      },
	{"Command Prompt",               "c", "\\",     "command",             },

	{0, 0, 0, 0},
	{"BTech",                        "c", "\\battle\\inceptio", "btech",   },
	{"BTech 2",                      "c", "\\battle\\revenge",  "revenge", },
	{"Mechwarrior",                  "c", "\\battle\\mechwar",  "mw",      },
	{"Mines of Titan",               "c", "\\titan",            "titan",   },

	{0, 0, 0, 0},
	{"Syndicate",                    "c", "\\syndplus\\sb16",   "synd",    },
	{"Ultima Underworld",            "d", "\\",                 "uw.bat",  },

	{0, 0, 0, 0},
	{"Pool of Radiance",             "c", "\\wizworx\\poolrad",  "st",     },
	{"Curse of the Azure Bonds",     "c", "\\wizworx\\curse",    "start",  },
	{"Hillsfar",                     "c", "\\hillsfar",          "main",   },
	{"Secret of the Silver Blades",  "c", "\\wizworx\\secret",   "start",  },
	{"Dark Queen of Krynn",          "c", "\\wizworx\\dqk",      "start",  },
	{"Death Knights of Krynn",       "c", "\\wizworx\\dkk",      "start",  },
	{"Champions of Krynn",           "c", "\\wizworx\\cok",      "start",  },
	{"Gateway to the Savage Frontier",
	                                 "c", "\\wizworx\\gateway",  "start",  },
	{"Treasures of the Savage Frontier",
	                                 "c", "\\wizworx\\treasure", "start",  },
	{"Pools of Darkness",            "c", "\\wizworx\\pools",    "start",  },
};

enum {
	ITEMS = sizeof(menu) / sizeof(menu[0]),

	HIKEY = 0xff00,

	UP    = HIKEY | 0x48,
	LEFT  = HIKEY | 0x4b,
	RIGHT = HIKEY | 0x4d,
	DOWN  = HIKEY | 0x50,

	HOME  = HIKEY | 0x47,
	END   = HIKEY | 0x4f,
	PGUP  = HIKEY | 0x49,
	PGDN  = HIKEY | 0x51,

	INS   = HIKEY | 0x52,
	DEL   = HIKEY | 0x53,

	F1    = HIKEY | 0x3b,
	F2    = HIKEY | 0x3c,
	F3    = HIKEY | 0x3d,
	F4    = HIKEY | 0x3e,
	F5    = HIKEY | 0x3f,
	F6    = HIKEY | 0x40,
	F7    = HIKEY | 0x41,
	F8    = HIKEY | 0x42,
	F9    = HIKEY | 0x43,
	F10   = HIKEY | 0x44,
	F11   = HIKEY | 0x45,
	F12   = HIKEY | 0x46,
};

int HORIZONTAL = 0;
struct text_info ti;
int menu_rows = 1, menu_cols = 1;
char buf[256] = "";
int input_len = 0, offset = 0;

void run(int which)
{
	if (menu[which].drive && menu[which].dir && menu[which].run) {
		int olddisk = getdisk();
		clrscr();
		setdisk(menu[which].drive[0] - 'a');
		chdir(menu[which].dir);
		system(menu[which].run);
		clrscr();
		setdisk(olddisk);
		chdir("\\");
	}
}

void prompt()
{
	clrscr();
	system("command");
	clrscr();
}

int longest()
{
	int i, max = 1, len = 1;
	for (i = 0; i < ITEMS; ++i) {
		len = strlen(menu[i].friendly);
		max = len > max ? len : max;
	}
	return max;
}

void draw(int selected)
{
	int x, y, i, w, sx = 0, sy = 0, top = 4, left = 3, padding;

	w = longest() + 3;
	menu_cols = ti.screenwidth / w ;
	padding = (ti.screenwidth - menu_cols * w - left - 1) / (menu_cols);
	w += padding;

	textcolor(LIGHTGRAY);
	textbackground(BLACK);

	menu_rows = (ITEMS + menu_cols - 1) / menu_cols;

	if (menu_rows < 25 - top)
		textmode(C80);
	else
		textmode(C4350);
	gettextinfo(&ti);

	for (i = 0, x = left, y = top; i < ITEMS; ++i) {
		gotoxy(x, y);
		if (i == selected) {
			textcolor(YELLOW);
			textbackground(BLUE);
			sx = x + 1; sy = y;
		} else {
			textcolor(LIGHTGRAY);
			textbackground(BLACK);
		}
		if (menu[i].friendly)
			cprintf(" %*s ", 3 - w, menu[i].friendly);
		/* Now adjust x and y cleverly */
		if (HORIZONTAL) {
			if ((x += w) >= ti.screenwidth - w) {
				x = left;
				++y;
			}
		} else {
			if ((++y) >= top + menu_rows) {
				y = top;
				x += w;
			}
		}
	}
	gotoxy(sx + offset, sy);
	textcolor(YELLOW);
	textbackground(RED);
	cprintf("%*s", input_len, buf);
	textcolor(LIGHTGRAY);
	textbackground(BLACK);
}

int biggetch()
{
	int c = getch();
	return c == 0 ? HIKEY | getch() : c;
}

int find_str(char *str, int len, int start)
{
	int which, where, w;
	w = longest();
	for (where = 0; where < w - len; ++where)
		for (which = start; which < ITEMS; ++which) {
			if (where >= strlen(menu[which].friendly))
				continue;
			if (0 == strncmpi(menu[which].friendly + where, str, len)) {
				offset = where;
				return which;
			}
		}
	return -1;
}

void clrinp()
{
	while (input_len > 0)
		buf[--input_len] = 0;
	offset = 0;
}

int main()
{
	int i, sel = 0, c, oldmode;

	gettextinfo(&ti);

	clrscr();
	for (;;) {
		draw(sel);
		switch ((c = biggetch())) {
			case 27:
				clrscr();
				textmode(LASTMODE);
				return 0;
				break;

			case '!':
				prompt();
				break;

			case DOWN:
				clrinp();
				if (HORIZONTAL) {
					if (sel < ITEMS - menu_cols) sel += menu_cols;
				} else {
					if (sel < ITEMS - 1) ++sel;
				}
				break;

			case RIGHT:
			case PGDN:
			case '+':
				clrinp();
				if (HORIZONTAL) {
					if (sel < ITEMS - 1) ++sel;
				} else {
					sel = (sel + menu_rows) % ITEMS;
				}
				break;

			case UP:
				clrinp();
				if (HORIZONTAL) {
					if (sel >= menu_cols) sel -= menu_cols;
				} else {
					if (sel > 0) --sel;
				}
				break;

			case LEFT:
			case PGUP:
			case '-':
				clrinp();
				if (HORIZONTAL) {
					if (sel > 0) --sel;
				} else {
					sel = (sel + ITEMS - menu_rows) % ITEMS;
				}
				break;

			case HOME:
				clrinp();
				sel = 0;
				break;

			case END:
				clrinp();
				sel = ITEMS - 1;
				break;

			case '\t':
				HORIZONTAL = !HORIZONTAL;
				clrscr();
				break;

			case '\r':
			case '\n':
			case ' ':
				run(sel);
				break;

			case '\b':
				if (input_len > 0) {
					buf[--input_len] = 0;
					if (input_len > 0)
						sel = find_str(buf, input_len, sel);
					else
						offset = 0;
				}
				break;

			default:
				if (  'a' <= c && c <= 'z'
					|| 'A' <= c && c <= 'Z'
					|| '0' <= c && c <= '9') {
					int found;
					buf[input_len++] = c;
					found = find_str(buf, input_len, sel);
					if (found<0)
						found = find_str(buf, input_len, 0);
					if (found < 0) {
						buf[--input_len] = 0;
					} else {
						sel = found;
					}
				} else {
					gotoxy(2, 20);
					cprintf("  %04X  ", c);
				}
				break;
		}
	}
	return 0;
}

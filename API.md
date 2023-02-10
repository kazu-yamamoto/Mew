				<API>
			    Kazu Yamamoto
			     Feb 16, 2004

---
* '"'

A command specified by '"' is called with "Grep pattern" and file
names of the messages in the directory of a target folder. "Grep
pattern" is encoded according to 'mew-cs-database-for-arg'.

The command must produce output whose lines start with a file name of
matched messages. It's OK if the same file name appears multiple
times.

	10: xxx pattern xxx
	10: yyy pattern yyy
	21: zzz pattern zzz

---
* mew-scan-form-*

A symbol, say 'symb', in 'mew-scan-form' means to call the function
'mew-scan-form-symb'. Read the document of 'mew-scan-form' for more
information. This function MUST return a string.

mew-scan-form-* can use some functions (MEW-FOO) and variables (TOTAL,
WIDTH).

Users customizable variables are 'mew-scan-fields' and
'mew-scan-fields-alias'.

'mew-scan-fields' MUST start with ("Folder:" "Filename:"). (nthcdr 2
mew-scan-fields) passed to "mewl" and 'mew-vec', which contains each
value of field in 'mew-scan-fields', is created.

Each element of 'mew-scan-fields-alias' is corresponding to an element
of 'mew-scan-fields', respectively. Functions called MEW-FOO is
defined according to 'mew-scan-fields-alias'.

	e.g.
		(MEW-SUBJ) returns the value of Subject:
		;; MEW-SUBJ is defined as (aref mew-vec 3)

Pre-defined functions are:
	MEW-FLD  - Folder:
	MEW-NUM  - Filename:
	MEW-SUBJ - Subject:
	MEW-DATE - Date:
	MEW-FROM - From:
	MEW-TO   - To:
	MEW-CT	 - Content-Type:
	MEW-ID   - Message-ID:
	MEW-UIDL - X-Mew-UIDL:

To get a size in X-Mew-UIDL:, call (mew-scan-get-size (MEW-UIDL)).
To get a uidl in X-Mew-UIDL:, call (mew-scan-get-uidl (MEW-UIDL)).

Also, two local variables are available: 
	TOTAL - the current total width
	WIDTH - the width

After you change both 'mew-scan-fields' and 'mew-scan-fields-alias',
call 'mew-status-update' ("Z" in Summary mode).

---
* Local mailbox

Mew uses POP to retrieve messages. If you want to retrieve them from a
local mailbox, set 'mew-mailbox-type' to 'mbox.

	(setq mew-mailbox-type 'mbox)

In this case, Mew executes "mewl" with 'mew-mbox-command' and
'mew-mbox-command-arg'.

	mewl -e mew-mbox-command -m mew-mbox-command-arg +folder

"mewl" then executes 'mew-mbox-command' like this:

	mew-mbox-command mew-mbox-command-arg +folder

Note that 'mew-mbox-command' is executed in the destination directory
(e.g. +folder). So, 'mew-mbox-command' need not to expand/analysis the
destination folder string.

Note also that you can specifies a source of messages (i.e. local
mailbox) in 'mew-mbox-command-arg'. (The source need not to be the
folder format. Just a path is enough.)

'mew-mbox-command' MUST be move a message to the destination, then
print its file name (a message number) to stdout. Repeat this cycle
until the mailbox becomes empty. 

"mewl" reads stdin, extracts a message number, opens the message,
then prints necessary fields. The extraction of a message number is
performed like "^[\t]*\\([0-9]+=\\)".

It is emphasized that 'mew-mbox-command' MUST print a resulting
message number just after it moves the corresponding file so that
"mewl" prints necessary fields immediately. It is a quite bad idea
that 'mew-mbox-command' moves all files first, then prints all message
numbers.

An example:

	(setq mew-mbox-command "inc")
	(setq mew-mbox-command-arg "-truncate -file /var/mail/user")

The image of this example is as follows:

	mewl -e inc -m '-truncate -file /var/mail/user' +inbox
                            v
                          (fork)
                            v
	inc -truncate -file /var/mail/user +inbox | mewl

---

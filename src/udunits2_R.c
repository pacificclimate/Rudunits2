/*
  James Hiebert <hiebert@uvic.ca>
  Pacific Climate Impacts Consortium
  August, 16, 2010

  Functions to support the R interface to the udunits (API version 2) library
*/

#include <R.h>
#include <udunits2.h>
#include <stdio.h> /* FILENAME_MAX */

typedef struct Sys_List{
    ut_system *sys;  //Pointer to the system
    ut_encoding enc; //Encoding for the system
    char *sys_name; //The name of the system
    struct Sys_List *next; //Pointer to the next node

}Sys_List;

struct Sys_List * head = NULL;//Pointer to the head of the list
int Total_Sys = 0; // Total number of systems added/loaded

/* From the enum comments in udunits2.h */
const char * ut_status_strings[] =
{
    "Success",
    "An argument violates the function's contract",
    "Unit, prefix, or identifier already exists",
    "No such unit exists",
    "Operating-system error.  See \"errno\".",
    "The units belong to different unit-systems",
    "The operation on the unit(s) is meaningless",
    "The unit-system doesn't have a unit named \"second\"",
    "An error occurred while visiting a unit",
    "A unit can't be formatted in the desired manner",
    "string unit representation contains syntax error",
    "string unit representation contains unknown word",
    "Can't open argument-specified unit database",
    "Can't open environment-specified unit database",
    "Can't open installed, default, unit database",
    "Error parsing unit specification"
};

void handle_error(const char *calling_function)
{
    ut_status stat;
    stat = ut_get_status();
    error("Error in function %s: %s", calling_function, ut_status_strings[stat]);
}


/*Function to check if a system is defined or not, if the system is defined then
it will return TRUE(or 1), else FALSE(or 0) will be returned */
void R_ut_has_system(int *exists, char * const *sys_name)
{
    Sys_List * current_sys_node;
    int found = 1;
	size_t length = strlen(*sys_name);

    if(head == NULL || sys_name == NULL) //If there are no systems defined or system name is null
    {
        *exists = 0;
        return;
    }

    current_sys_node = head;


    while(current_sys_node != NULL)
    {
        found = strncmp(*sys_name, current_sys_node->sys_name, length);

        if(found == 0) // if the system name is found
        {
            *exists = 1;
            return;
        }
        else//if system name is not found.
        {
            current_sys_node = current_sys_node->next;
        }
    }


    *exists = 0;
    return;
}

/*Function to check if a system is defined or not, if the system is defined then
the system will be returned, else a null value will be returned */
void R_ut_get_system(char *sys_name, Sys_List **named_sys)
{
    Sys_List * current_sys_node;
    int found = 1;
	size_t length = strlen(sys_name);

    if(head == NULL || sys_name == NULL) //If there are no systems defined or system name is null
    {
        *named_sys = NULL;
        error("System '%s' not defined.", sys_name);
        return;
    }

    *named_sys = NULL;
    current_sys_node = head;
    while(current_sys_node != NULL)
    {
        found = strncmp(sys_name, current_sys_node->sys_name, length);

        if(found == 0) // if the system name is found
        {
            *named_sys = current_sys_node;
            break;
        }
        else//if system name is not found.
        {
            current_sys_node = current_sys_node->next;
        }
    }

    if(current_sys_node == NULL)
    {
        error("System '%s' not defined.", sys_name);
    }

    return;
}


void R_ut_init(char * const *file_name, char * const *sys_name, const int *print_warning_on_failure)
{
    ut_status stat;

    char ** system_name;

    int *exist = NULL;

    Sys_List * current_sys_node, *new_sys_node;

    current_sys_node = new_sys_node = NULL;

    ut_set_error_message_handler((ut_error_message_handler) Rvprintf);


    //If system name is blank then the system is named as default
    if(sys_name == NULL || *sys_name == NULL || strlen(*sys_name) == 0)
    {
        system_name = (char**) R_alloc(1, sizeof(char*));
        *system_name = (char*) R_alloc(8, sizeof(char));
        strcpy(*system_name, "default");
    }
    else
    {
        system_name = (char**) R_alloc(1, sizeof(char*));
        *system_name = (char*) R_alloc((strlen(*sys_name)+1), sizeof(char));
        strcpy(*system_name, *sys_name);

    }


    if (head == NULL) // No system have yet been loaded
    {
        new_sys_node = (Sys_List *) Calloc(sizeof(Sys_List), Sys_List);
        head = new_sys_node;
    }
    else //traverse the list to check if system with same name exist, if not add a new system in the list
    {
        exist = (int *) Calloc(sizeof(int), int);

        //Check if a system with same name exist
        //R_ut_has_system(exist, sys_name);
        R_ut_has_system(exist, system_name);

        //If system with same name does not exist then add the system to the end of the system list
        //Else give an error
        if(*exist == 0)
        {
            current_sys_node = head;

            while(current_sys_node->next != NULL)
            {
                current_sys_node = current_sys_node->next;
            }
            new_sys_node = (Sys_List *) Calloc(sizeof(Sys_List), Sys_List);
            current_sys_node->next = new_sys_node;

            Free(exist);
        }
        else// System with same name exists give user the error message
        {
            error("System '%s' already defined.", *system_name);
            Free(exist);
            return;
        }

    }

    new_sys_node->next = NULL;
    new_sys_node->enc = UT_UTF8;//Default encoding

    //Copy system name to the system list
    new_sys_node->sys_name = (char*)Calloc((strlen(*system_name)+1) * sizeof(char), char);

    strcpy(new_sys_node->sys_name, *system_name);

    ut_set_error_message_handler(ut_ignore);


    if(file_name == NULL || *file_name == NULL || strlen(*file_name) == 0)
    {
        new_sys_node->sys = ut_read_xml(NULL);
    }
    else
    {
        new_sys_node->sys = ut_read_xml(*file_name);
    }

    ut_set_error_message_handler((ut_error_message_handler) Rvprintf);

    if (new_sys_node->sys == NULL)//If system creation fails
    {
        stat = ut_get_status();

        if (*print_warning_on_failure)
            ut_handle_error_message("Warning in R_ut_init: %s\n", ut_status_strings[stat]);

        if(current_sys_node != NULL)
            current_sys_node->next = NULL;

        if(head == new_sys_node)//If head was pointing to the new system
            head = NULL;

        Free(new_sys_node->sys_name);
        Free(new_sys_node);

        return;
    }

    Total_Sys ++; //Increase the count of systems


    return;
}



/* Take an encoding string and set the global var enc *///--updated for multiple systems
/*Take an encoding string and set the encoding for the unit system given by the user */
void R_ut_set_encoding(const char * const *enc_string, char * const *sys_name)
{
    Sys_List *current_sys_node = NULL;

    size_t length = strlen(*enc_string);

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }

    if(current_sys_node == NULL)
    {
        return;
    }

    if (strncmp(*enc_string, "utf8", length) == 0)
    {
        current_sys_node->enc = UT_UTF8;
    }
    else if (strncmp(*enc_string, "ascii", length) == 0)
    {
        current_sys_node->enc = UT_ASCII;
    }
    else if (strncmp(*enc_string, "iso-8859-1", length) == 0 ||
             strncmp(*enc_string, "latin1", length) == 0)
    {
        current_sys_node->enc = UT_LATIN1;
    }
    else
    {
        error("Valid encoding string parameters are ('utf8'|'ascii'|'iso-8859-1','latin1')");
    }


    return;
}

void R_ut_is_parseable(char * const *units_string, int *parseable, char * const *sys_name)
{
    ut_unit *result;
    //int one = 1;
    Sys_List *current_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);

    }

    if (current_sys_node == NULL)//<TODO:>
    {
        return;
    }

    ut_trim(*units_string, current_sys_node->enc);
    result = ut_parse(current_sys_node->sys, *units_string, current_sys_node->enc);

    if (result == NULL)
    {
        *parseable = 0;
    }
    else
    {
        *parseable = 1;
    }

    ut_free(result);

    return;
}

void R_ut_are_convertible(char * const *ustring1, char * const *ustring2,
                          int *convertible, char * const *sys_name)
{
    ut_unit *u1, *u2;

    Sys_List *current_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }


    if (current_sys_node == NULL)//<TODO:>
    {
        return;
    }

    ut_trim(*ustring1, current_sys_node->enc);
    ut_trim(*ustring2, current_sys_node->enc);
    u1 = ut_parse(current_sys_node->sys, *ustring1, current_sys_node->enc);
    u2 = ut_parse(current_sys_node->sys, *ustring2, current_sys_node->enc);

    if (!(u1 && u2))
    {
        handle_error("R_ut_are_convertible");
    }

    if (ut_are_convertible(u1, u2) == 0)
    {
        *convertible = 0;
    }
    else
    {
        *convertible = 1;
    }

    ut_free(u1);
    ut_free(u2);
    return;
}

void R_ut_convert(const double *x, int *count, char * const *units_from,
                  char * const *units_to, double *rv, char * const *sys_name)
{
    ut_unit *from, *to;
    cv_converter *conv;

    Sys_List *current_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }

    if (current_sys_node == NULL)//<TODO:>
    {
        return;
    }

    ut_trim(*units_from, current_sys_node->enc);
    ut_trim(*units_to, current_sys_node->enc);

    from = ut_parse(current_sys_node->sys, *units_from, current_sys_node->enc);
    if (from == NULL)
    {
        handle_error("R_ut_convert");
        return;
    }

    to = ut_parse(current_sys_node->sys, *units_to, current_sys_node->enc);
    if (from == NULL)
    {
        handle_error("R_ut_convert");
        return;
    }
    conv = ut_get_converter(from, to);
    if (conv == NULL)
    {
        handle_error("R_ut_convert");
        return;
    }

    cv_convert_doubles(conv, x, (size_t) *count, rv);

    // Cleanup
    cv_free(conv);
    ut_free(to);
    ut_free(from);
    return;
}

void R_ut_get_name(char * const *ustring, char **rstring, char * const *sys_name)
{
    ut_unit *u;
    char *trimmed;
    char *s;
    Sys_List *current_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }

    if (current_sys_node == NULL)
    {
        return;
    }

    trimmed = ut_trim(*ustring, current_sys_node->enc);
    u = ut_parse(current_sys_node->sys, trimmed, current_sys_node->enc);

    if (!u)
    {
        handle_error("R_ut_get_name");
    }

    s = (char *) ut_get_name(u, current_sys_node->enc); // FIXME: ut_get_name seems to allocate the string... does it need to be free-ed?

    if (s == NULL) return;
    else *rstring = s;

    return;
}

void R_ut_get_symbol(char * const *ustring, char **rstring, char * const *sys_name)
{
    ut_unit *u;
    char *trimmed;
    char *s;

    Sys_List *current_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }


    if (current_sys_node == NULL)
    {
        return;
    }

    trimmed = ut_trim(*ustring, current_sys_node->enc);
    u = ut_parse(current_sys_node->sys, trimmed, current_sys_node->enc);

    if (!u)
    {
        handle_error("R_ut_get_symbol");
    }

    s = (char *) ut_get_symbol(u, current_sys_node->enc); // FIXME: ut_get_symbol seems to allocate the string... does it need to be free-ed?

    if (s == NULL) return;
    else *rstring = s;

    return;
}


/*Function to remove a defined unit system*/
void R_ut_free_system(char * const *sys_name, int* deleted)
{
    Sys_List *current_sys_node, *prev_sys_node;
    current_sys_node = prev_sys_node = NULL;

    if(sys_name != NULL )
    {
        //Check if the system name is defined or not
        R_ut_get_system(*sys_name, &current_sys_node);
    }


    if(current_sys_node == NULL)//If the name is not defined
    {
        *deleted = 0;
        return;
    }


    if(current_sys_node == head)//If the system is the first system in the list
    {
        head = head->next;
    }
    else//If the system is not the first system on the list go to 1 node prior to the system containing name
    {
        prev_sys_node = head;

        while(prev_sys_node->next != current_sys_node)
        {
            prev_sys_node = prev_sys_node->next;
        }

        prev_sys_node->next = current_sys_node->next;
    }

    //Free the system
    ut_free_system(current_sys_node->sys);

    //Free the system name
    Free(current_sys_node->sys_name);

    //Deallocate memory from the list
    Free(current_sys_node);

    //Decrease the count of systems
    Total_Sys --;

    //Set deleted to true
    *deleted = 1;

    return;
}

/*Get the total number of defined systems*/
void R_ut_system_count(int *count)
{
    *count = Total_Sys;
    return;
}


/*Function to get the list of system names*/
void R_ut_list_systems(char ** SystemList)
{
    Sys_List * current_sys_node = NULL;

    int count = 0;

    if(head == NULL)//If no system is yet defined.
    {
        SystemList = NULL;
        return;
    }

    current_sys_node = head;

    for(count = 0; count < Total_Sys; count++)//Traverse the system linked list and get the names
    {
        SystemList[count] = (char*) R_alloc((strlen(current_sys_node->sys_name)+1), sizeof(char));
        strcpy(SystemList[count], current_sys_node->sys_name);
        current_sys_node = current_sys_node->next;
    }

    return;
}

/*Function to delete the complete list of system.*/
void R_ut_system_cleanup()
{
    Sys_List *current_sys_node = NULL;

    while(head != NULL)
    {
        current_sys_node = head;
        head = head->next;

        ut_free_system(current_sys_node->sys);
        Free(current_sys_node->sys_name);
        Free(current_sys_node);
        current_sys_node = NULL;
    }

    Total_Sys = 0;
    return;

}

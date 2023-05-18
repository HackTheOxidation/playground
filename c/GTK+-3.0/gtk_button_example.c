#include <stdio.h>
#include <stdlib.h>
#include <gtk/gtk.h>


// Print function for button
static void print_hello(GtkWidget *widget, gpointer data) {
	g_print("Hello There!\n");
}

// Creates and activates main window and widget layout
static void activate(GtkApplication* app, gpointer user_data) {
	// Pointer to the main window
	GtkWidget *window; 
	GtkWidget *button;
	GtkWidget *button_box;
	
	// Creates the main window
	window = gtk_application_window_new(app);
	// Sets the window title
	gtk_window_set_title(GTK_WINDOW (window), "Main Window");
	// Sets the window size
	gtk_window_set_default_size(GTK_WINDOW (window), 200, 300);


	// Add further widgets here:
	
	// Creates a horizontal button box (layout container for boxes, javafx hbox)
	button_box = gtk_button_box_new(GTK_ORIENTATION_HORIZONTAL);
	
	// Adds the button_box to the main window
	gtk_container_add(GTK_CONTAINER (window), button_box);

	
	
	// Creates a button with label: "Click Me!"
	button = gtk_button_new_with_label("Click Me!");
	
	// Connects the button to the print function
	g_signal_connect(button, "clicked", G_CALLBACK (print_hello), NULL);

	// Connects (swapped) the button to the gtk widget destroy function i.e. The main window is destroyed when clicking the button 
	g_signal_connect_swapped(button, "clicked", G_CALLBACK (gtk_widget_destroy), window);

	// Adds the buttoin to the button box	
	gtk_container_add(GTK_CONTAINER (button_box), button);


	// Shows the main window
	gtk_widget_show_all(window);
}


// Main function: Creates and destroys the GTK application
int main(int argc, char **argv) {
	// Pointer to the main GTK application
	GtkApplication *app;
	
	// Exit status
	int status;
	
	// Creates the main GTK Application
	app = gtk_application_new("org.gtk.example", G_APPLICATION_FLAGS_NONE);

	// Connects the app pointer to and activates main window
	g_signal_connect(app, "activate", G_CALLBACK (activate), NULL);
	
	// Gets the exit code
	status = g_application_run(G_APPLICATION (app), argc, argv);
	
	// Frees the GTK Application from memory
	g_object_unref(app);

	return status;
}

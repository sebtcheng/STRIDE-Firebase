# ==========================================================
# 00_firebase_patch.R - DEBUG MODE
# ==========================================================
library(R6)

Firebase <- R6::R6Class("Firebase",
                        public = list(
                          config = NULL,
                          
                          initialize = function(config) {
                            print("PATCH: Firebase Class Initialized")
                            self$config <- config
                          },
                          
                          sign_in = function(email, password) {
                            print(paste("PATCH: sign_in called for:", email))
                            session <- shiny::getDefaultReactiveDomain()
                            if (is.null(session)) {
                              print("PATCH ERROR: Session is NULL! Cannot send message.")
                            } else {
                              print("PATCH: Sending 'firebase-sign_in' message to JS...")
                              session$sendCustomMessage("firebase-sign_in", list(
                                email = email, 
                                password = password
                              ))
                            }
                          },
                          
                          create_user = function(email, password) {
                            print(paste("PATCH: create_user called for:", email))
                            session <- shiny::getDefaultReactiveDomain()
                            session$sendCustomMessage("firebase-create_user", list(
                              email = email, 
                              password = password
                            ))
                          },
                          
                          sign_out = function() {
                            print("PATCH: sign_out called")
                            session <- shiny::getDefaultReactiveDomain()
                            session$sendCustomMessage("firebase-sign_out", list())
                          },
                          
                          get_signed_in = function() {
                            input <- shiny::getDefaultReactiveDomain()$input
                            val <- input$fire_signed_in
                            if (!is.null(val)) print("PATCH: get_signed_in retrieved value from JS")
                            val
                          },
                          
                          get_created = function() {
                            input <- shiny::getDefaultReactiveDomain()$input
                            input$fire_created
                          }
                        )
)
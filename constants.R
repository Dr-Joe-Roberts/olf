# constants.R - Define constants for the application
OLFACTOMETER_TYPES <- list(
  "Four-Arm Olfactometer" = "four", 
  "Two-Arm Olfactometer" = "two", 
  "Six-Arm Olfactometer" = "six"
)

DEFAULT_ODOURS <- list(
  "four" = c("Odour A", "Odour B", "Odour C", "Odour D"),
  "two" = c("Odour A", "Odour B"),
  "six" = c("Odour A", "Odour B", "Odour C", "Odour D", "Odour E", "Odour F")
)

# Consolidated JavaScript for key handling
KEY_HANDLER_JS <- "
function attachKeyListener() {
  document.addEventListener('keydown', keyListener);
}
function detachKeyListener() {
  document.removeEventListener('keydown', keyListener);
}
function keyListener(event) {
  // Customize key handling based on current olfactometer type
  const olfactometerType = document.getElementById('olfactometer_type').value;
  let validKeys = ['1', '2', '3', '4', '5'];
  
  if (olfactometerType === 'six') {
    validKeys = ['1', '2', '3', '4', '5', '6', '7'];
  } else if (olfactometerType === 'two') {
    validKeys = ['1', '2', '5'];
  }
  
  if(validKeys.includes(event.key)) {
    Shiny.onInputChange('key', {key: event.key, time: new Date()});
  } else {
    alert('Invalid key! Please press a key from the valid range for this olfactometer type.');
  }
}

// Handle visibility changes to ensure key listener is properly attached/detached
document.addEventListener('visibilitychange', function() {
  if (document.visibilityState === 'visible') {
    attachKeyListener();
  } else {
    detachKeyListener();
  }
});

Shiny.addCustomMessageHandler('attachKeyListener', function(message) {
  attachKeyListener();
});
Shiny.addCustomMessageHandler('detachKeyListener', function(message) {
  detachKeyListener();
});
"
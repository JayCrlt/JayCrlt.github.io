<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<title>Untitled Document</title>
</head>
<body>
<?php

// define variables and set to empty values
$nameErr = $emailErr = $subjectErr = $messageErr = "";
$name = $email = $subject = $message = "";
if ($_SERVER["REQUEST_METHOD"] == "POST") {
 

if (empty($_POST["name"])) {
   
$nameErr = "Name is required";
 
} else {
   
$name = test_input($_POST["name"]);
   
// check if name only contains letters and whitespace
   
if (!preg_match("/^[a-zA-Z-' ]*$/",$name)) {
     
$nameErr = "Only letters and white space allowed";
   
}
 
}
 
if (empty($_POST["email"])) {
   

$emailErr = "Email is required";
 
} else {
   
$email = test_input($_POST["email"]);
   
// check if e-mail address is well-formed
   
if (!filter_var($email, FILTER_VALIDATE_EMAIL)) {
     
$emailErr = "Invalid email format";
   
}
 
}

 
if (empty($_POST["subject"])) {
   
$subjectErr = "Subject is required";
 
} else {
   
$subject = test_input($_POST["subject"]);
   
}
 
}

 
if (empty($_POST["message"])) {
   
$message = "Message is required";
 
} else {
   
$message = test_input($_POST["message"]);
  }

?>
</body>
</html>
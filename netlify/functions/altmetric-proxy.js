export async function handler(event) {
  const id = event.queryStringParameters.id;
  if (!id) {
    return {
      statusCode: 400,
      body: JSON.stringify({ error: "Missing Altmetric ID" }),};}
  const url = `https://api.altmetric.com/details/${id}`;
  try {
    const response = await fetch(url);
    if (!response.ok) {throw new Error("Altmetric API returned error");}
    const data = await response.json();
    return {statusCode: 200, headers: {
      "Access-Control-Allow-Origin": "*",
      "Content-Type": "application/json"},
      body: JSON.stringify(data),};} catch (error) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: "Failed to fetch Altmetric" }),};}}
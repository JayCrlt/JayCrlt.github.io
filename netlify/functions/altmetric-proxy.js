// netlify/functions/altmetric-proxy.js
export async function handler(event) {
  const id = event.queryStringParameters.id;

  const url = `https://api.altmetric.com/details/${id}`;

  try {
    const response = await fetch(url);
    const data = await response.json();

    return {
      statusCode: 200,
      headers: {
        "Access-Control-Allow-Origin": "*",
        "Content-Type": "application/json"
      },
      body: JSON.stringify(data)
    };
  } catch (err) {
    return {
      statusCode: 500,
      body: JSON.stringify({ error: "Failed to fetch Altmetric" })
    };
  }
}
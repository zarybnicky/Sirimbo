export const origin = typeof window === 'undefined'
  ? (process.env.GRAPHQL_BACKEND || `http://localhost:${process.env.PORT || 3000}`) : '';

export const fetcher = <TData, TVariables>(
  query: string,
  variables?: TVariables,
  options?: RequestInit['headers']
): (() => Promise<TData>) => {
  return async () => {
    const res = await fetch(origin + '/graphql', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        ...options
      },
      body: JSON.stringify({ query, variables }),
    })

    const json = await res.json()

    if (json.errors) {
      const { message } = json.errors[0] || {}
      throw new Error(message || 'Error…')
    }

    return json.data
  }
}

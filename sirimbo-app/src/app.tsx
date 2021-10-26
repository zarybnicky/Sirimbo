import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { ApolloClient, ApolloProvider, InMemoryCache, HttpLink, useQuery } from '@apollo/client';
import { RozpisDocument } from './queries';
import { useState } from 'react';

export const App = () => {
    const [client] = useState(() => new ApolloClient({
        link: new HttpLink({ uri: '/graphql/v1/graphql' }),
        cache: new InMemoryCache(),
    }));
    return (
        <ApolloProvider client={client}>
            <Rozpis id={1287}></Rozpis>
        </ApolloProvider >
    );
};
const Rozpis = ({ id }: { id: number; }) => {
    const { loading, error, data } = useQuery(RozpisDocument, { variables: { id } });
    if (loading) {
        return <div>Loading...</div>;
    }
    if (error) {
        console.error(error);
        return <div>Error!</div>;
    }
    return <div>{data}</div>;
};
class AppElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<App />, this);
    }
}
customElements.define('app', AppElement);

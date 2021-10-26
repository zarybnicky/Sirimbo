import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState, useEffect } from 'react';
import buildHasuraProvider from 'ra-data-hasura';
import { Admin, Resource, ListGuesser } from 'react-admin';

const NoticeboardAdmin = () => {
    const [dataProvider, setDataProvider] = useState(null);
    useEffect(() => {
        (async () => {
            const dataProvider = await buildHasuraProvider({
                clientOptions: { uri: '/graphql/v1/graphql' }
            });
            setDataProvider(() => dataProvider);
        })()
    }, []);
    if (!dataProvider) return <p>Loading...</p>;

    return (
        <Admin dataProvider={dataProvider}>
            <Resource name="upozorneni" list={ListGuesser} />
        </Admin>
    );
};

class NoticeboardAdminElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(<NoticeboardAdmin></NoticeboardAdmin>, this);
    }
}
customElements.define('noticeboard-admin', NoticeboardAdminElement);

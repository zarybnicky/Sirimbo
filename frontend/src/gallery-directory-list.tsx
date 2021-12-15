import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, useQuery, useMutation } from '@apollo/client';
import { createClient } from './client';
import { Pagination } from './pagination';
import { gql } from 'graphql-tag';
import { Dropdown } from './dropdown';

const GalleryDirList = gql(`
  query GalleryDirList($offset: Int, $limit: Int) {
    galerie_dir(limit: $limit, offset: $offset, order_by: {gd_name: asc}) {
      gd_hidden
      gd_id
      gd_id_rodic
      gd_level
      gd_name
      gd_path
    }
    aggregate: galerie_dir_aggregate {
      aggregate {
        count
      }
    }
  }
`);

const ToggleVisible = gql(`
  mutation setGalerieDirVisible($id: bigint!, $visible: Boolean!) {
    update_galerie_dir_by_pk(pk_columns: {gd_id: $id}, _set: {gd_hidden: $visible}) {
      gd_id
    }
  }
`);

type Treeified<T> = T & { id: number; parentId: number; children: Treeified<T>[]; };
function listToTree<T>(list: Treeified<T>[]) {
    const map: { [k: number]: number } = {};
    const roots = []
    for (let i = 0; i < list.length; i += 1) {
        map[list[i].id] = i;
    }
    for (let i = 0; i < list.length; i += 1) {
        if (list[i].parentId === list[i].id) {
            roots.push(list[i]);
        } else {
            list[map[list[i].parentId]].children.push(list[i]);
        }
    }
    return roots;
}
function flatten<T>(root: Treeified<T>): T[] {
    const output: T[] = [];
    const stack = [root];
    while (stack.length > 0) {
        let node = stack.pop()!!;
        output.push(node);
        node.children.forEach((x) => stack.push(x))
    }
    return output;
}

export function GalleryDirectoryList() {
    const [limit, setLimit] = useState(30);
    const [offset, setOffset] = useState(0);
    const [total, setTotal] = useState(0);
    const { data, refetch } = useQuery(GalleryDirList, {
        variables: { limit, offset },
        onCompleted: (data) => {
            const total = data.aggregate?.aggregate?.count;
            total && setTotal(total);
        },
    });
    const roots = listToTree((data?.galerie_dir || []).map(x => ({
        ...x,
        id: x.gd_id,
        parentId: x.gd_id_rodic,
        children: [],
    })));
    const dataSorted = roots.length > 0 ? flatten(roots[0]) : [];
    const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
    const [toggleVisible] = useMutation(ToggleVisible, {
        onCompleted: () => refetch(),
    });

    const list = !dataSorted.length ? null : <table>
        <thead><tr><th>Složka</th><th>Skrytá</th></tr></thead>
        <tbody>
            {dataSorted.map((a) => <tr key={a.gd_id}>
                <td>
                    <Dropdown links={{
                        [`/admin/galerie/directory/edit/${a.gd_id}`]: "Upravit",
                        [`/admin/galerie/directory/${a.gd_id}`]: "Upravit fotky",
                        [`/admin/galerie/directory/remove/${a.gd_id}`]: "Odstranit",
                    }} />
                    {'→'.repeat(a.gd_level - 1)} {a.gd_name}
                </td>
                <td>
                    <Form.Check checked={a.gd_hidden} onChange={() => toggleVisible({
                        variables: { id: a.gd_id, visible: !a.gd_hidden },
                    })} />
                </td>
            </tr>)}
        </tbody>
    </table>;

    return <React.Fragment>
        <a className="btn btn-outline-primary" href="/admin/galerie/file/upload">Přidat fotky</a>
        <a className="btn btn-outline-primary" href="/admin/galerie/directory/add">Přidat složku</a>
        {list}
        <Pagination {...{ total, limit, setPage }} />
    </React.Fragment>;
}

export class GalleryDirectoryListElement extends HTMLElement {
    connectedCallback() {
        ReactDOM.render(
            <ApolloProvider client={createClient()}><GalleryDirectoryList /></ApolloProvider>,
            this
        );
    }
}

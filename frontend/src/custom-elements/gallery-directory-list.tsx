import * as React from 'react';
import * as ReactDOM from 'react-dom';
import { useState } from 'react';
import Form from 'react-bootstrap/Form';
import { ApolloProvider, HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { Pagination } from './pagination';
import { $, GalerieDirsOrderBy, Selector } from '../zeus';
import { useTypedQuery, useTypedMutation } from '../zeus/apollo';
import { Dropdown } from './dropdown';

const GalleryDirList = Selector('Query')({
  galerieDirs: [
    { first: $`limit`, offset: $`offset`, orderBy: [GalerieDirsOrderBy.GD_NAME_ASC] },
    {
      nodes: {
        gdHidden: true,
        gdId: true,
        gdIdRodic: true,
        gdLevel: true,
        gdName: true,
        gdPath: true,
      },
      totalCount: true,
    }
  ],
});

const ToggleVisible = Selector('Mutation')({
  updateGalerieDirByNodeId: [
    { input: { nodeId: $`id`, patch: { gdHidden: $`visible` } } },
    {
      galerieDir: {
        gdId: true,
      },
    },
  ],
});

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
  const { data, refetch } = useTypedQuery(GalleryDirList, {
    variables: { limit, offset },
    onCompleted: (data) => {
      const total = data.galerieDirs?.totalCount;
      total && setTotal(total);
    },
  });
  const roots = listToTree((data?.galerieDirs?.nodes || []).map(x => ({
    ...x,
    id: x.gdId,
    parentId: x.gdIdRodic,
    children: [],
  })));
  const dataSorted = roots.length > 0 ? flatten(roots[0]) : [];
  const setPage = (x: { selected: number; }) => setOffset(x.selected * limit);
  const [toggleVisible] = useTypedMutation(ToggleVisible, {
    onCompleted: () => refetch(),
  });

  const list = !dataSorted.length ? null : <table>
    <thead><tr><th>Složka</th><th>Skrytá</th></tr></thead>
    <tbody>
      {dataSorted.map((a) => <tr key={a.gdId}>
        <td>
          <Dropdown links={{
            [`/admin/galerie/directory/edit/${a.gdId}`]: "Upravit",
            [`/admin/galerie/directory/${a.gdId}`]: "Upravit fotky",
            [`/admin/galerie/directory/remove/${a.gdId}`]: "Odstranit",
          }} />
          {'→'.repeat(a.gdLevel - 1)} {a.gdName}
        </td>
        <td>
          <Form.Check checked={a.gdHidden} onChange={() => toggleVisible({
            variables: { id: a.gdId, visible: !a.gdHidden },
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

const client = new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export class GalleryDirectoryListElement extends HTMLElement {
  connectedCallback() {
    ReactDOM.render(
      <ApolloProvider client={client}><GalleryDirectoryList /></ApolloProvider>,
      this
    );
  }
}

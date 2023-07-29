import * as React from 'react';
import { Pagination } from '@app/ui/Pagination';
import { Dropdown } from './dropdown';
import { useQuery, useMutation } from 'urql';
import {
  GalleryDirListDocument,
  ToggleGalleryDirVisibleDocument,
} from '@app/graphql/Gallery';

type Treeified<T> = T & { id: number; parentId: number; children: Treeified<T>[] };
function listToTree<T>(list: Treeified<T>[]) {
  const map: { [k: number]: number } = {};
  const roots = [];
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
    node.children.forEach((x) => stack.push(x));
  }
  return output;
}

export default function GalleryDirectoryList() {
  const [page, setPage] = React.useState(1);
  const [{ data }, refetch] = useQuery({
    query: GalleryDirListDocument,
    variables: { first: 10, offset: (page - 1) * 10 },
  });
  const [_, toggleVisible] = useMutation(ToggleGalleryDirVisibleDocument);

  const roots = listToTree(
    (data?.galerieDirs?.nodes || []).map((x) => ({
      ...x,
      id: parseInt(x.id, 10),
      parentId: parseInt(x.gdIdRodic, 10),
      children: [],
    })),
  );
  const dataSorted = roots.length > 0 ? flatten(roots[0]) : [];

  return (
    <>
      <a className="btn btn-outline-primary" href="/admin/galerie/file/upload">
        Přidat fotky
      </a>
      <a className="btn btn-outline-primary" href="/admin/galerie/directory/add">
        Přidat složku
      </a>
      <table>
        <thead>
          <tr>
            <th>Složka</th>
            <th>Skrytá</th>
          </tr>
        </thead>
        <tbody>
          {dataSorted.map((a) => (
            <tr key={a.id}>
              <td>
                <Dropdown
                  links={{
                    [`/admin/galerie/directory/edit/${a.id}`]: 'Upravit',
                    [`/admin/galerie/directory/${a.id}`]: 'Upravit fotky',
                    [`/admin/galerie/directory/remove/${a.id}`]: 'Odstranit',
                  }}
                />
                {'→'.repeat(a.gdLevel - 1)} {a.gdName}
              </td>
              <td>
                <input
                  type="checkbox"
                  checked={a.gdHidden}
                  onChange={async () => {
                    await toggleVisible({ id: a.id.toString(), hidden: !a.gdHidden });
                    refetch();
                  }}
                />
              </td>
            </tr>
          ))}
        </tbody>
      </table>
      {!!data?.galerieDirs?.totalCount && (
        <Pagination
          total={data?.galerieDirs?.totalCount}
          limit={10}
          page={page}
          setPage={setPage}
        />
      )}
    </>
  );
}

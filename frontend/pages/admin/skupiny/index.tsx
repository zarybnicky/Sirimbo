import { Dropdown } from "components/Dropdown";
import { NextLinkComposed } from "components/Link";
import { useCohortListQuery } from "lib/graphql";
import { useRequireUserLoggedIn } from "lib/route-guards";

export default function CohortsPage() {
  useRequireUserLoggedIn();

  const { data } = useCohortListQuery();

  return <>
    <NextLinkComposed href="/admin/skupiny/add" className="btn btn-primary">Nová skupina</NextLinkComposed>
    <table>
      <thead>
        <tr><th>Jméno</th><th>Přidáno</th></tr>
      </thead>
      <tbody>
        {data?.skupinies?.nodes?.map((row) => <tr key={row.sId}>
          <td>
            <Dropdown
              button={<>
                <div className="box" title={row.sDescription} style={{ backgroundColor: row.sColorRgb }} />
                {row.sName}{row.sLocation && `, ${row.sLocation}`}
                {!row.sVisible && ` (skrytá)`}
              </>}
              options={[
                { title: 'Upravit', href: `/admin/skupiny/edit/${row.sId}` },
                { title: 'Odstranit', href: `/admin/skupiny/remove/${row.sId}` },
              ]}
            />
          </td>
        </tr>)}
      </tbody>
    </table>
  </>;
}

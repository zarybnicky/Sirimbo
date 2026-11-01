import { compareAgeRanges, formatAgeGroup, formatAgeRange, formatAgeRangeForYear } from '@/ui/csts';
import { AgeGroupRulesDocument } from '@/graphql/AgeGroups';
import { executeGraphql } from '@/lib/server/graphql';
import { Table } from '@/ui/Table';

const standardTempo = [
  ['Waltz', '28–30'],
  ['Tango', '31–33'],
  ['Valčík', '58–60'],
  ['Slowfoxtrot', '28–30'],
  ['Quickstep', '50–52'],
] as const;

const latinTempo = [
  ['Samba', '50–52'],
  ['Chacha', '30–32'],
  ['Rumba', '25–27'],
  ['Paso doble', '60–62'],
  ['Jive', '42–44'],
  ['Polka', '58–60'],
] as const;

const classDances = [
  ['Do 8 let', 'E', '-', '-', '3T: Wa, Ch, Po'],
  ['Děti I, II', 'E', '-', '-', '5T: Wa, Qs, Ch, Ji, Po'],
  ['', 'D', 'Wa, Va, Qs', 'Sa, Ch, Ji, Po', '7T'],
  ['', 'C', 'Wa, Ta, Va, Qs', 'Sa, Ch, Ru, Ji', '8T'],
  ['Junioři I a výše', 'E', 'Wa, Va, Qs', 'Sa, Ch, Ji', '6T'],
  ['', 'D', 'Wa, Ta, Va, Qs', 'Sa, Ch, Ru, Ji', '8T'],
  ['', 'C, B, A, M, P', 'Wa, Ta, Va, Qs, Sf, Qs', 'Sa, Ch, Ru, Pd, Ji', '10T'],
] as const;

export default async function ToolsPage() {
  const year = new Date().getFullYear();
  const { ageGroupsList } = await executeGraphql(AgeGroupRulesDocument);
  const categories = (ageGroupsList ?? [])
    .flatMap(({ coupleRules, code, name }) =>
      coupleRules.map((rule) => ({ code, name, ...rule })),
    )
    .toSorted((a, b) => compareAgeRanges(a.olderAge, b.olderAge));

  return (
    <main className="min-h-screen bg-neutral-1 text-neutral-12">
      <article className="prose prose-accent mx-auto max-w-5xl px-6 py-10">
        <h1>Tempo tanců</h1>

        <div className="flex w-full flex-wrap justify-stretch gap-2">
          {[
            { name: 'STT', tempos: standardTempo },
            { name: 'LAT', tempos: latinTempo },
          ].map(({ name, tempos }) => (
            <Table key={name} columns={[name, 'Taktů/min']} className="flex-1">
              {tempos.map(([dance, tempo]) => (
                <tr key={dance}>
                  <td>{dance}</td>
                  <td>{tempo}</td>
                </tr>
              ))}
            </Table>
          ))}
        </div>

        <h1>Tance v třídách</h1>

        <Table columns={['Kategorie', 'Třída', 'STT', 'LAT', 'Kombi']}>
          {classDances.map(([category, danceClass, standard, latin, combined], i) => (
            <tr key={`${category || 'same'}-${danceClass}-${i}`}>
              <td>{category}</td>
              <td>{danceClass}</td>
              <td>{standard}</td>
              <td>{latin}</td>
              <td>{combined}</td>
            </tr>
          ))}
        </Table>

        <h1>Věkové kategorie</h1>

        <Table
          columns={[
            'Kategorie',
            { label: 'Starší partner', colSpan: 2 },
            { label: 'Mladší partner', colSpan: 2 },
          ]}
        >
          {categories.map((c) => (
            <tr key={c.code}>
              <td>{formatAgeGroup(c)}</td>
              <td>
                <span className="inline-block bg-accent-3 m-1 py-1 px-2 rounded-md whitespace-nowrap">
                  {formatAgeRange(c.olderAge)}
                </span>
              </td>
              <td>{formatAgeRangeForYear(year, c.olderAge)}</td>
              <td>
                <span className="inline-block bg-accent-3 m-1 py-1 px-2 rounded-md whitespace-nowrap">
                  {formatAgeRange(c.youngerAge)}
                </span>
              </td>
              <td>{formatAgeRangeForYear(year, c.youngerAge)}</td>
            </tr>
          ))}
        </Table>
      </article>
    </main>
  );
}

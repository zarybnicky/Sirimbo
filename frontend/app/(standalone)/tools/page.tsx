import { AgeCategoryTable } from '@/ui/AgeCategoryTable';

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

/* eslint-disable import-x/no-unused-modules */
export default function ToolsPage() {
  return (
    <main className="min-h-screen bg-neutral-1 text-neutral-12">
      <article className="prose prose-accent mx-auto max-w-5xl px-6 py-10">
        <h1>Tempo tanců</h1>

        <div className="flex w-full flex-wrap justify-stretch gap-2">
          <div className="flex-1">
            <table>
              <thead>
                <tr>
                  <th>STT</th>
                  <th>Taktů/min</th>
                </tr>
              </thead>
              <tbody>
                {standardTempo.map(([dance, tempo]) => (
                  <tr key={dance}>
                    <td>{dance}</td>
                    <td>{tempo}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>

          <div className="flex-1">
            <table>
              <thead>
                <tr>
                  <th>LAT</th>
                  <th>Taktů/min</th>
                </tr>
              </thead>
              <tbody>
                {latinTempo.map(([dance, tempo]) => (
                  <tr key={dance}>
                    <td>{dance}</td>
                    <td>{tempo}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>

        <h1>Tance v třídách</h1>

        <table>
          <thead>
            <tr>
              <th>Kategorie</th>
              <th>Třída</th>
              <th>STT</th>
              <th>LAT</th>
              <th>Kombi</th>
            </tr>
          </thead>
          <tbody>
            {classDances.map(([category, danceClass, standard, latin, combined], i) => (
              <tr key={`${category || 'same'}-${danceClass}-${i}`}>
                <td>{category}</td>
                <td>{danceClass}</td>
                <td>{standard}</td>
                <td>{latin}</td>
                <td>{combined}</td>
              </tr>
            ))}
          </tbody>
        </table>

        <h1>Věkové kategorie</h1>

        <AgeCategoryTable />
      </article>
    </main>
  );
}

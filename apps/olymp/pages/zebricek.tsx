import React from 'react';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from 'components/layout/Layout';
import { useQuery } from 'urql';
import { ScoreboardDocument } from '@app/graphql/Scoreboard';
import Link from 'next/link';

const Page = () => {
  const [{ data }] = useQuery({ query: ScoreboardDocument })

  return (
    <Layout requireMember>
      <TitleBar title="Žebříček (work-in-progress)" />

      <div className="prose prose-accent">
      <p> Momentálně počítá pouze současné aktivní členy, ale napříč všemi daty v systému od začátku roku 2022. </p>
      <p> Pokud vidíte nesrovnalosti mezi partnery (např. Šimon vs Katy), nejspíš se jedná o indivky proběhlé v době, kdy jste v systému byli špatně spárovaní. Později dodám detailnější výpis toho, za co jste dostali body, zatím ale takto.</p>
      <p> Skóre se prozatím skládá pouze z: 1 společná = 2b, 1 indivka = 3b </p>

      <table>
        <thead>
          <tr>
            <th></th>
            <th>Člen</th>
            <th>Indivky</th>
            <th>Společné</th>
            <th>Celkové skóre</th>
          </tr>
        </thead>
        <tbody>
          {data?.scoreboardsList?.map(x => (
            <tr key={x.personId}>
              <td>{x.ranking}.</td>
              <td>
                <Link href={`/clenove/${x.personId}`}>
                  {x.person?.name}
                </Link>
              </td>
              <td>{x.lessonTotalScore}</td>
              <td>{x.groupTotalScore}</td>
              <td>{x.totalScore}</td>
            </tr>
          ))}
        </tbody>
      </table>
      </div>
    </Layout>
  );
};

export default Page;

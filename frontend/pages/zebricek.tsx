import React from 'react';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from '@/components/layout/Layout';
import { useQuery } from 'urql';
import { ScoreboardDocument } from '@app/graphql/Scoreboard';
import Link from 'next/link';

const Page = () => {
  const [{ data }] = useQuery({ query: ScoreboardDocument })

  return (
    <Layout requireMember>
      <TitleBar title="Žebříček aktivity" />

      <div className="prose prose-accent">
        <p>Skóre se skládá z:</p>
        <dl className="text-sm">
          <dt>Individuální lekce</dt>
          <dd>1b, max. 4b za týden</dd>
          <dt>Pohybovka, performance</dt>
          <dd>1b</dd>
          <dt>Vedená hodina</dt>
          <dd>1b</dd>
          <dt>Practice</dt>
          <dd>2b</dd>
          <dt>Jednodenní klubová akce</dt>
          <dd>3b</dd>
          <dt>Vícedenní klubová akce</dt>
          <dd>5b</dd>
          <dt>Účast na soutěži</dt>
          <dd><s>2b</s></dd>
          <dt>Sportovní aktivita</dt>
          <dd><s>1b</s></dd>
        </dl>

        <table>
          <thead>
            <tr>
              <th></th>
              <th>Člen</th>
              <th className="text-center">Indivky/společné/akce</th>
              <th className="text-center">Celkem</th>
            </tr>
          </thead>
          <tbody>
            {data?.scoreboardsList?.map(x => (
              <tr
                key={x.personId}
                className={
                  x.ranking === '1' ? 'bg-yellow-200 text-black' :
                  x.ranking === '2' ? 'bg-sky-200 text-black' :
                  x.ranking === '3' ? 'bg-orange-200 text-black' :
                  ''
                }
              >
                <td><b>{x.ranking}.</b></td>
                <td>
                  <Link href={`/clenove/${x.personId}`}>
                    {x.person?.name}
                  </Link>
                </td>
                <td className="text-center">
                  {x.lessonTotalScore}/{x.groupTotalScore}/{x.eventTotalScore}
                </td>
                <td className="text-center font-bold">
                  {x.totalScore}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </Layout>
  );
};

export default Page;

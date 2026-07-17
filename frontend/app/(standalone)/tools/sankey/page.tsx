/* eslint-disable import-x/no-unused-modules */

import type { Metadata } from 'next';
import { ascending, range, scaleOrdinal, schemeTableau10 } from 'd3';
import { sankey, sankeyLinkHorizontal } from 'd3-sankey';
import type { SankeyNode as D3SankeyNode, SankeyLink as D3SankeyLink } from 'd3-sankey';
import { runQuery } from '@/lib/server/postgresql';
import {
  competitionSankeyLinks,
  type ICompetitionSankeyLinksResult,
} from '../tools.queries';

const KIND_LABEL: Record<string, string> = {
  entry: 'entry',
  flow: 'active',
  dropout: 'dropout',
  inactive: 'inactive',
  return: 'return',
};
const STATE_ORDER: Record<string, number> = { active: 0, inactive: 1, hidden: 9 };
const AXIS_VALUE = 1e-6;

type LinkExtra = {
  id: string;
  discipline: string;
  kind: string;
  hidden?: boolean;
};

type SankeyNode = {
  id: string;
  year: number;
  state: string;
  class: string | null;
  label: string;
  xIndex: number;
  classIndex: number;
  hidden?: boolean;
};

type LayoutNodeBase = D3SankeyNode<SankeyNode, LinkExtra>;

type LayoutNode = LayoutNodeBase &
  Required<Pick<LayoutNodeBase, 'x0' | 'x1' | 'y0' | 'y1'>>;

type LayoutLink = Omit<
  D3SankeyLink<SankeyNode, LinkExtra>,
  'source' | 'target' | 'width'
> & {
  source: LayoutNode;
  target: LayoutNode;
  width: number;
};

const indexBy = <T,>(values: T[]) =>
  new Map(values.map((value, index) => [value, index]));

function buildGraph(
  rows: ICompetitionSankeyLinksResult[],
  {
    classes,
    fromYear,
    toYear,
  }: {
    classes: string[];
    fromYear: number;
    toYear: number;
  },
) {
  const years = range(fromYear, toYear + 1);
  const yearIndex = indexBy(years);
  const classRank = indexBy(classes);

  const nodesById = new Map<string, SankeyNode>();
  const addNode = (year: number, state: string, cls: string | null) => {
    const id = `${year}:${state}:${cls ?? '__all'}`;
    if (!nodesById.has(id)) {
      nodesById.set(id, {
        id,
        year,
        state,
        class: cls,
        label: cls ?? 'Inactive',
        xIndex: yearIndex.get(year)!,
        classIndex: cls == null ? 9999 : classRank.get(cls)!,
      });
    }
    return id;
  };

  // rows arrive unique and in render order; nodes are just their distinct endpoints
  const links = rows.map((row) => {
    const source = addNode(row.fromYear, row.fromState, row.fromClass);
    const target = addNode(row.toYear, row.toState, row.toClass);
    return {
      id: `${source}|${target}|${row.discipline}|${row.kind}`,
      source,
      target,
      value: row.value,
      discipline: row.discipline,
      kind: row.kind,
    };
  });

  return {
    nodes: [
      ...nodesById.values(),
      ...years.map((year) => ({
        id: `${year}:hidden:__axis__`,
        year,
        state: 'hidden',
        class: null,
        label: '',
        xIndex: yearIndex.get(year)!,
        classIndex: 10_000,
        hidden: true,
      })),
    ] satisfies SankeyNode[],
    links: [
      ...links,
      ...years.slice(0, -1).map((year, index) => ({
        id: `${year}:axis`,
        source: `${year}:hidden:__axis__`,
        target: `${years[index + 1]}:hidden:__axis__`,
        value: AXIS_VALUE,
        discipline: '__axis__',
        kind: 'axis',
        hidden: true,
      })),
    ],
    years,
    disciplines: [...new Set(rows.map((row) => row.discipline))].toSorted(),
  };
}

async function CompetitionSankey({
  width = 960,
  height = 560,
  mergeInactive = false,
  disciplines,
  classes,
  fromYear,
  toYear,
}: {
  width?: number;
  height?: number;
  mergeInactive?: boolean;
  disciplines: string[];
  classes: string[];
  fromYear: number;
  toYear: number;
}) {
  const rows = await runQuery(competitionSankeyLinks, {
    firstYear: fromYear,
    lastYear: toYear,
    disciplines,
    classes,
    mergeInactive,
  });

  if (rows.length === 0) return <p>No competition flows for this selection.</p>;

  const graphData = buildGraph(rows, { classes, fromYear, toYear });

  const graph = sankey<SankeyNode, LinkExtra>()
    .nodeId((node) => node.id)
    .nodeAlign((node, columnCount) =>
      Math.max(0, Math.min(node.xIndex, columnCount - 1)),
    )
    .nodeSort(function (a, b) {
      return (
        ascending(a.classIndex, b.classIndex) ||
        ascending(STATE_ORDER[a.state], STATE_ORDER[b.state]) ||
        ascending(a.label, b.label)
      );
    })
    .nodeWidth(14)
    .nodePadding(16)
    .linkSort(null)
    .extent([
      [120, 34],
      [width - 28, height - 34],
    ])({
    nodes: graphData.nodes.map((node) => ({ ...node })),
    links: graphData.links.map((link) => ({ ...link })),
  }) as {
    nodes: LayoutNode[];
    links: LayoutLink[];
  };

  const color = scaleOrdinal(graphData.disciplines, schemeTableau10);
  const path = sankeyLinkHorizontal();
  const visibleNodes = graph.nodes.filter((node) => !node.hidden);
  const visibleLinks = graph.links.filter((link) => !link.hidden);

  return (
    <figure>
      <svg
        viewBox={`0 0 ${width} ${height}`}
        role="img"
        aria-label="Competition class movement Sankey"
      >
        <g>
          {graphData.years.map((year) => {
            const node = graph.nodes.find((candidate) => candidate.year === year);
            return (
              <text
                key={year}
                x={node ? (node.x0 + node.x1) / 2 : 0}
                y="20"
                textAnchor="middle"
                fontSize="12"
              >
                {year}
              </text>
            );
          })}
        </g>

        <g fill="none" strokeOpacity="0.48">
          {visibleLinks.map((link) => (
            <path
              key={link.id}
              d={path(link)!}
              stroke={color(link.discipline)}
              strokeWidth={Math.max(1, link.width!)}
              strokeDasharray={
                link.kind === 'flow'
                  ? undefined
                  : link.kind === 'inactive'
                    ? '2 3'
                    : '6 4'
              }
            >
              <title>{`${link.source.label} ${link.source.year} -> ${link.target.label} ${link.target.year}\n${link.value} ${link.discipline}; ${KIND_LABEL[link.kind]}`}</title>
            </path>
          ))}
        </g>

        <g>
          {visibleNodes.map((node) => (
            <g key={node.id}>
              <rect
                x={node.x0}
                y={node.y0}
                width={node.x1 - node.x0}
                height={Math.max(1, node.y1 - node.y0)}
                rx="2"
                fill={node.state === 'inactive' ? '#d8d8d8' : '#666'}
                stroke="white"
              >
                <title>{`${node.label}\n${node.year}; ${node.state}`}</title>
              </rect>
              <text
                x={node.x0 - 7}
                y={(node.y0 + node.y1) / 2}
                dy="0.35em"
                textAnchor="end"
                fontSize="11"
              >
                {node.label}
              </text>
            </g>
          ))}
        </g>
      </svg>

      <figcaption style={{ display: 'flex', gap: 12, flexWrap: 'wrap', fontSize: 12 }}>
        {graphData.disciplines.map((discipline) => (
          <span key={discipline}>
            <span
              aria-hidden="true"
              style={{
                display: 'inline-block',
                width: 10,
                height: 10,
                marginRight: 4,
                background: color(discipline),
              }}
            />
            {discipline}
          </span>
        ))}
      </figcaption>
    </figure>
  );
}

export const dynamic = 'force-dynamic';

export const metadata: Metadata = {
  title: 'Competition sankey',
  robots: { index: false },
};

export default async function Page() {
  return (
    <CompetitionSankey
      disciplines={['Standard', 'Latin']}
      classes={['E', 'D', 'C', 'B', 'A', 'S']}
      fromYear={2024}
      toYear={2026}
      mergeInactive={false}
    />
  );
}

'use client';

import { useEffect, useMemo, useState } from 'react';
import { ascending, max, min, range, scaleOrdinal, schemeTableau10 } from 'd3';
import { sankey, sankeyLinkHorizontal } from 'd3-sankey';
import type { SankeyNode as D3SankeyNode, SankeyLink as D3SankeyLink } from 'd3-sankey';
import { origin } from '@/lib/query';

const KIND_LABEL = {
  entry: 'entry',
  flow: 'active',
  dropout: 'dropout',
  inactive: 'inactive',
  return: 'return',
  axis: 'axis',
};
const KIND_ORDER = ['entry', 'flow', 'dropout', 'inactive', 'return'];
const STATE_ORDER = { active: 0, inactive: 1, hidden: 9 };
const AXIS_VALUE = 1e-6;

type SankeyItem = {
  from_year: number;
  from_state: 'active' | 'inactive';
  from_class: string;
  to_year: number;
  to_state: 'active' | 'inactive';
  to_class: string;
  discipline: string;
  kind: 'flow' | 'dropout' | 'inactive' | 'return' | 'entry';
  value: number;
};

type LinkExtra = {
  id: string;
  discipline: string;
  kind: 'flow' | 'dropout' | 'inactive' | 'return' | 'entry' | 'axis';
  fromYear: number;
  toYear: number;
  fromClass: string;
  toClass: string;
  hidden?: boolean;
};

type SankeyNode = {
  id: string;
  year: number;
  state: 'active' | 'inactive' | 'hidden';
  class: string | null | undefined;
  label: string;
  xIndex: number;
  classIndex: number;
  hidden?: boolean;
};

type SankeyLink = LinkExtra & {
  source: string;
  target: string;
  value: number;
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

const indexBy = <T,>(values: T[] = []) =>
  new Map(values.map((value, index) => [value, index]));

function buildGraph(
  rows: SankeyItem[] = [],
  {
    mergeInactive,
    classes,
    fromYear,
    toYear,
  }: {
    mergeInactive?: boolean;
    classes: string[];
    fromYear: number;
    toYear: number;
  },
) {
  const cleanRows = rows.filter((row) => Number(row.value) > 0);
  if (cleanRows.length === 0) return { nodes: [], links: [], years: [], disciplines: [] };

  classes = classes?.length
    ? classes
    : [
        ...new Set(
          cleanRows.flatMap((row) => [row.from_class, row.to_class]).filter(Boolean),
        ),
      ].toSorted();
  const classRank = indexBy(classes);
  const linksById = new Map<string, SankeyLink>();

  for (const row of cleanRows) {
    const fromClass =
      mergeInactive && row.from_state === 'inactive' ? '_all' : row.from_class;
    const toClass =
      mergeInactive && row.from_state === 'inactive' ? '_all' : row.to_class;
    const source = `${row.from_year}:${row.from_state}:${fromClass}`;
    const target = `${row.to_year}:${row.to_state}:${toClass}`;
    const id = `${source}|${target}|${row.discipline}|${row.kind}`;
    const link = linksById.get(id);

    if (link) {
      link.value += Number(row.value);
    } else {
      linksById.set(id, {
        id,
        source,
        target,
        value: Number(row.value),
        discipline: row.discipline,
        kind: row.kind,
        fromYear: Number(row.from_year),
        toYear: Number(row.to_year),
        fromClass: row.from_class,
        toClass: row.to_class,
      });
    }
  }

  const links = [...linksById.values()].toSorted(
    (a, b) =>
      ascending(a.fromYear, b.fromYear) ||
      ascending(classRank.get(a.fromClass), classRank.get(b.fromClass)) ||
      ascending(KIND_ORDER.indexOf(a.kind), KIND_ORDER.indexOf(b.kind)) ||
      ascending(a.discipline, b.discipline) ||
      ascending(a.toYear, b.toYear) ||
      ascending(classRank.get(a.toClass), classRank.get(b.toClass)),
  );

  const firstYear =
    fromYear ?? min(links, (link) => Math.min(link.fromYear, link.toYear));
  const lastYear = toYear ?? max(links, (link) => Math.max(link.fromYear, link.toYear));
  const years = range(firstYear, lastYear + 1);
  const yearIndex = indexBy(years);
  const nodes = [...new Set(links.flatMap((link) => [link.source, link.target]))].map(
    (id) => {
      const [yearText, state, classText] = id.split(':');
      const merged = state === 'inactive' && classText === '__all';
      const cls = merged ? null : classText;

      return {
        id,
        year: Number(yearText),
        state: state as 'active' | 'inactive',
        class: cls,
        label: merged ? 'Inactive' : cls!,
        xIndex: yearIndex.get(Number(yearText))!,
        classIndex: cls == null ? 9999 : classRank.get(cls)!,
      };
    },
  );

  return {
    nodes: [
      ...nodes,
      ...years.map((year) => ({
        id: `${year}:hidden:__axis__`,
        year,
        state: 'hidden' as const,
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
        kind: 'axis' as const,
        fromYear: year,
        toYear: years[index + 1]!,
        fromClass: '',
        toClass: '',
        hidden: true,
      })),
    ],
    years,
    disciplines: [
      ...new Set(cleanRows.map((row) => row.discipline).filter(Boolean)),
    ].toSorted(),
  };
}

export function CompetitionSankey({
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
  const [rows, setRows] = useState<SankeyItem[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<unknown | null>(null);

  useEffect(() => {
    async function load() {
      setLoading(true);
      setError(null);

      try {
        const response = await fetch(`/rpc/competition_sankey_links`, {
          method: 'POST',
          headers: {
            Accept: 'application/json',
            'Content-Type': 'application/json',
          },
          body: JSON.stringify({
            p_first_year: fromYear,
            p_last_year: toYear,
            p_disciplines: disciplines,
            p_classes: classes,
          }),
        });

        if (!response.ok)
          throw new Error(
            (await response.text()) || `Request failed with ${response.status}`,
          );

        setRows(await response.json());
      } catch (err) {
        setError(err);
      } finally {
        setLoading(false);
      }
    }

    load();
  }, [classes, disciplines, fromYear, toYear]);

  const graphData = useMemo(
    () => buildGraph(rows, { mergeInactive, classes, fromYear, toYear }),
    [rows, mergeInactive, classes, fromYear, toYear],
  );

  const graph = useMemo(() => {
    if (graphData.nodes.length === 0) return { nodes: [], links: [] };

    return sankey<SankeyNode, LinkExtra>()
      .nodeId((node) => node.id)
      .nodeAlign((node, columnCount) =>
        Math.max(0, Math.min(node.xIndex ?? 0, columnCount - 1)),
      )
      .nodeSort(function (a, b) {
        return (
          ascending(a.classIndex, b.classIndex) ||
          ascending(STATE_ORDER[a.state] ?? 5, STATE_ORDER[b.state] ?? 5) ||
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
  }, [graphData, width, height]);

  const color = useMemo(
    () => scaleOrdinal(graphData.disciplines, schemeTableau10),
    [graphData.disciplines],
  );
  const path = useMemo(() => sankeyLinkHorizontal(), []);
  const visibleNodes = graph.nodes.filter((node) => !node.hidden);
  const visibleLinks = graph.links.filter((link) => !link.hidden);

  if (loading) return <p>Loading competition flows...</p>;
  if (error)
    return <p role="alert">Could not load competition flows: {JSON.stringify(error)}</p>;
  if (visibleLinks.length === 0) return <p>No competition flows for this selection.</p>;

  return (
    <figure className="competition-sankey">
      <svg
        viewBox={`0 0 ${width} ${height}`}
        role="img"
        aria-label="Competition class movement Sankey"
      >
        <g className="year-labels">
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
              <title>{`${link.source.label} ${link.source.year} -> ${link.target.label} ${link.target.year}\n${link.value} ${link.discipline}; ${KIND_LABEL[link.kind] || link.kind}`}</title>
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

export default function Page() {
  return (
    <CompetitionSankey
      disciplines={['Standard', 'Latin']}
      classes={['E', 'D', 'C', 'B', 'A', 'S']}
      fromYear={2024}
      toYear={2026}
    />
  );
}

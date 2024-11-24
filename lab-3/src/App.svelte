<script lang="ts">
  import { Header, HeaderNav, HeaderNavItem } from "carbon-components-svelte";

  import Terminal from "./lib/Terminal.svelte";
  import Modal from "./lib/Modal.svelte";
  import { onMount } from "svelte";
  import Graph from "./lib/Graph.svelte";
  import init, { parse_points, process } from "interpolator";

  import type {
    InterpolationResult,
    InterpolationType,
    Point,
  } from "./lib/types";
  import Parameters from "./lib/Parameters.svelte";

  let showModal = $state(false);

  // Terminal states
  let acquireLock = $state<() => void>();
  let releaseLock = $state<() => void>();
  let clearBuffer = $state<() => void>();
  let clearTerminal = $state<() => void>();
  let write = $state<(text: string) => Promise<void>>();
  let readCallback = $state<(text: string) => Promise<void>>();
  let resetCallback = $state<() => Promise<void>>();

  // Parameters states
  let locked = $state(false);
  let selectedMethods = $state<boolean[]>([true]);
  let selectedGraph = $state<string>("default");
  let methods = $state<InterpolationType[]>([]);
  let step = $state(1);

  // Graph states
  let points = $state<Point[]>([]);
  let resetParameters = $state<() => void>();
  let graph = $state<string>("");

  let generatedGraphs: any = $state({});

  const formatPoints = (points: Point[]) => {
    points = points.map((p) => ({
      x: ((p.x * 100) | 0) / 100,
      y: ((p.y * 100) | 0) / 100,
    }));

    const columnLengths = points.map((p) =>
      Math.max(p.x.toString().length + 2, p.y.toString().length + 2, 8)
    );
    let rowX = "";
    let rowY = "";

    for (let i = 0; i < points.length; i++) {
      rowX += points[i].x.toString().padEnd(columnLengths[i], " ");
      rowY += points[i].y.toString().padEnd(columnLengths[i], " ");
    }

    return `${rowX}\n\r${rowY}\n\r`;
  };

  const printResults = async (results: InterpolationResult[]) => {
    for (const result of results) {
      const displayName = methods.find(
        (m) => m.id === result.method
      )?.displayName;
      await write?.("\n\r");
      await write?.(`${displayName} results:\n\r`);
      await write?.(formatPoints(result.points));
    }
  };

  const onRead = async (text: string) => {
    try {
      acquireLock?.();
      let new_points: Point[] = await parse_points(text);
      new_points = [...points, ...new_points];
      const result: InterpolationResult[] = await process({
        points: new_points,
        step,
        methods: methods.filter((_, i) => selectedMethods[i]).map((m) => m.id),
      });
      await printResults(result);
      generatedGraphs = result.reduce<any>((acc, val): any => {
        acc[val.method] = val.latex_equation;
        return acc;
      }, {});
      points = new_points;
      locked = true;
      await write?.("\n\r");
    } catch (ex: unknown) {
      await write?.((ex as Error).message);
      await write?.("\n\r");
    } finally {
      await write?.("> ");
      releaseLock?.();
    }
  };

  const onReset = async () => {
    locked = false;
    graph = "";
    generatedGraphs = {};
    resetParameters?.();
    clearBuffer?.();
    clearTerminal?.();
    points = [];
    await write?.("FP Lab #3\n\r> ");
  };

  onMount(async () => {
    await init();
    await write?.("FP Lab #3\n\r> ");
    readCallback = onRead;
    resetCallback = onReset;
  });

  $effect(() => {
    let current;
    if (selectedGraph === "default") {
      current = Object.values(generatedGraphs)[0] as string;
    } else {
      current = generatedGraphs[selectedGraph];
    }

    graph = current ?? "";
  });
</script>

<Header company="Степанов А. А. " platformName="ФП Лаб. #3">
  <HeaderNav>
    <HeaderNavItem href="https://github.com/Armemius/FunctionalProgramming"
      >Репозиторий</HeaderNavItem
    >
    <HeaderNavItem
      href="https://github.com/Armemius/FunctionalProgramming/tree/main/lab-3"
      >Исходный код</HeaderNavItem
    >
    <HeaderNavItem on:click={() => (showModal = true)}>Помощь</HeaderNavItem>
  </HeaderNav>
</Header>

<Modal bind:showModal>
  {#snippet header()}
    <h3>Интерполяция функций</h3>
  {/snippet}

  {#snippet children()}
    <span class="leading-relaxed">
      Данная утилита позволяет интерполировать функции по заданному набору
      точек. Для получения интерполированных значений введите координаты точек в
      терминал
      <br />
      <br />
      По достижении необходимого количества точек начнётся расчёт и вывод значений.
      <br />
      <br />
      При помощи параметров можно выбрать методы, размер шага и построить интересующий
      график функции интерполяции.
      <br />
      <br />
      Терминал поддерживает следующие сочетания клавиш: <br />
      <strong>Ctrl + C</strong>: сброс параметров и перезапуск сессии <br />
      <strong>Ctrl + L</strong>: очистка терминал <br />
    </span>
  {/snippet}
</Modal>
<main
  class="grid grid-cols-[min-content_auto] gap-20 justify-items-center p-[70px_50px]"
>
  <div class="flex flex-col gap-5">
    <Terminal
      bind:acquireLock
      bind:releaseLock
      bind:write
      bind:clearBuffer
      bind:clearTerminal
      bind:readCallback
      bind:resetCallback
    />
    <Parameters
      bind:locked
      bind:selectedMethods
      bind:selectedGraph
      bind:reset={resetParameters}
      bind:methods
      bind:step
    />
  </div>
  <Graph {points} {graph} />
</main>

<script lang="ts">
  import { Header, HeaderNav, HeaderNavItem } from "carbon-components-svelte";

  import Terminal from "./lib/Terminal.svelte";
  import { onMount } from "svelte";
  import Graph from "./lib/Graph.svelte";
  import init, { parse_points, process } from "interpolator";
  import type { InterpolationResult, Point } from "./lib/types";
  import Parameters from "./lib/Parameters.svelte";


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

  // Graph states
  let points = $state<Point[]>([]);
  let resetParameters = $state<() => void>();

  const onRead = async (text: string) => {
    locked = true;
    try {
      acquireLock?.();
      let new_points: Point[];
      new_points = await parse_points(text);
      points = [...points, ...new_points];
      await process({
        points: points,
      });
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
    <HeaderNavItem
      on:click={() => alert("Перемоги!")}
      >Помощь</HeaderNavItem
    >
  </HeaderNav>
</Header>

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
      {readCallback}
      {resetCallback}
    />
    <Parameters 
      {locked}
      bind:selectedMethods
      bind:selectedGraph
      bind:reset={resetParameters}
    />
  </div>
  <Graph {points} />
</main>

using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class IntPair
{
    public int x;
    public int y;

    public IntPair(int x, int y)  {
        this.x = x;
        this.y = y;
    }

    public override bool Equals(object o)
    {
        return base.Equals (o);
    }

    public override int GetHashCode()
    {
        return x * y + x;
    }

    public IntPair Left() {
        return new IntPair(x - 1, y);
    }

    public IntPair Right() {
        return new IntPair(x + 1, y);
    }

    public IntPair Up() {
        return new IntPair(x, y - 1);
    }

    public IntPair Down() {
        return new IntPair(x , y + 1);
    }
}

public class IntPairComparer : IEqualityComparer<IntPair>
{
    public bool Equals(IntPair one, IntPair two) {
        return one.x == two.x && one.y == two.y;
    }

    public int GetHashCode(IntPair pair) {
        return pair.x * pair.y + pair.x;
    }
}

public class Slot : Object
{
    private IntPair pos;
    private Dictionary<IntPair, Slot> allSlots;

    public Slot(IntPair pos, Dictionary<IntPair, Slot> allSlots) {
        this.pos = pos;
        this.allSlots = allSlots;
    }

    private Slot otherSlot(IntPair pos) {
        return allSlots[pos];
    }

    public Slot Left() {
        return otherSlot(pos.Left());
    }

    public Slot Right() {
        return otherSlot(pos.Right());
    }

    public Slot Up() {
        return otherSlot(pos.Up ());
    }

    public Slot Down() {
        return otherSlot(pos.Down ());
    }
}

public class Board : Object
{
//    private int columns;
//    private int rows;

    public Board(int columns, int rows) {
//        this.columns = columns;
//        this.rows = rows;

        int count = rows * columns;
        Dictionary<IntPair, Slot> allSlots = new Dictionary<IntPair, Slot>(count, new IntPairComparer());
        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < columns; x++) {
                IntPair pair = new IntPair(x, y); 
                Slot slot = new Slot(pair, allSlots);
                allSlots.Add(pair, slot);
            }
        }
    }
}

public class GameController : MonoBehaviour
{
    public GameObject tileQuad;

    // Use this for initialization
    void Start()
    {


        GameObject topLeft = Instantiate (tileQuad, new Vector3 (-1.1f, 1.1f), Quaternion.identity) as GameObject;
        topLeft.renderer.material.mainTextureOffset = new Vector2 (0f, 0.66f);
        GameObject topMiddle = Instantiate (tileQuad, new Vector3 (0f, 1.1f), Quaternion.identity) as GameObject;
        topMiddle.renderer.material.mainTextureOffset = new Vector2 (0.33f, 0.66f);
        GameObject topRight = Instantiate (tileQuad, new Vector3 (1.1f, 1.1f), Quaternion.identity) as GameObject;
        topRight.renderer.material.mainTextureOffset = new Vector2 (0.66f, 0.66f);

        GameObject left = Instantiate (tileQuad, new Vector3 (-1.1f, 0f), Quaternion.identity) as GameObject;
        left.renderer.material.mainTextureOffset = new Vector2 (0f, 0.33f);


    }


    // Update is called once per frame
    void Update()
    {

    }
}

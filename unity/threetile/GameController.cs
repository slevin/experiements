using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class IntPair
{
    public int x;
    public int y;

    public IntPair(int x, int y)
    {
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

    public IntPair Left()
    {
        return new IntPair (x - 1, y);
    }

    public IntPair Right()
    {
        return new IntPair (x + 1, y);
    }

    public IntPair Up()
    {
        return new IntPair (x, y - 1);
    }

    public IntPair Down()
    {
        return new IntPair (x, y + 1);
    }
}

public class IntPairComparer : IEqualityComparer<IntPair>
{
    public bool Equals(IntPair one, IntPair two)
    {
        return one.x == two.x && one.y == two.y;
    }

    public int GetHashCode(IntPair pair)
    {
        return pair.x * pair.y + pair.x;
    }
}

public class Slot : Object
{
    private IntPair pos;
    private Dictionary<IntPair, Slot> allSlots;

    public Slot(IntPair pos, Dictionary<IntPair, Slot> allSlots)
    {
        this.pos = pos;
        this.allSlots = allSlots;
    }

    private Slot otherSlot(IntPair pos)
    {
        return allSlots [pos];
    }

    public Slot Left()
    {
        return otherSlot (pos.Left ());
    }

    public Slot Right()
    {
        return otherSlot (pos.Right ());
    }

    public Slot Up()
    {
        return otherSlot (pos.Up ());
    }

    public Slot Down()
    {
        return otherSlot (pos.Down ());
    }
}

public class Tile
{
    GameObject tilePrefab;
    GameObject tileObject;

    public Tile(GameObject tilePrefab)
    {
        this.tilePrefab = tilePrefab;
    }

    public void instantiateGameObject()
    {
        
    }
}

public class Board : Object
{
//    private int columns;
//    private int rows;

    public Board(int columns, int rows, GameObject tileQuad)
    {
//        this.columns = columns;
//        this.rows = rows;

        int count = rows * columns;
        Dictionary<IntPair, Slot> allSlots = new Dictionary<IntPair, Slot> (count, new IntPairComparer ());
        for (int y = 0; y < rows; y++) {
            for (int x = 0; x < columns; x++) {
                IntPair pair = new IntPair (x, y); 
                Slot slot = new Slot (pair, allSlots);
                allSlots.Add (pair, slot);
            }
        }
    }

    /*
     * fill with tile objects 
     * scramble (forget that)
     * instantiate each one
     * position based on its tile location, so there is some sizer that knows how to draw
     * */
}

public class GameController : MonoBehaviour
{
    public GameObject tilePrefab;
    public float tileSpacing;
    public int rows;
    public int columns;
    public float tileSize;

    // Use this for initialization
    void Start()
    {
        
        //Board board = new Board(2, 2);

        /*
         * board draw yourself
         * for each item in 
*/


        /*
        GameObject topLeft = Instantiate (tileQuad, new Vector3 (-1.1f, 1.1f), Quaternion.identity) as GameObject;
        topLeft.renderer.material.mainTextureOffset = new Vector2 (0f, 0.66f);
        GameObject topMiddle = Instantiate (tileQuad, new Vector3 (0f, 1.1f), Quaternion.identity) as GameObject;
        topMiddle.renderer.material.mainTextureOffset = new Vector2 (0.33f, 0.66f);
        GameObject topRight = Instantiate (tileQuad, new Vector3 (1.1f, 1.1f), Quaternion.identity) as GameObject;
        topRight.renderer.material.mainTextureOffset = new Vector2 (0.66f, 0.66f);

        GameObject left = Instantiate (tileQuad, new Vector3 (-1.1f, 0f), Quaternion.identity) as GameObject;
        left.renderer.material.mainTextureOffset = new Vector2 (0f, 0.33f);
*/

    }


    // Update is called once per frame
    void Update()
    {

    }

    public GameObject CreateTile(int positionX, int positionY)
    {
        float totalWidth = this.columns * this.tileSize + (this.columns - 1) * this.tileSpacing;
        float totalHeight = this.rows * this.tileSize + (this.rows - 1) * this.tileSpacing;
        float halfWidth = totalWidth / 2.0f;
        float halfHeight = totalHeight / 2.0f;
        float myX = this.tileSize + this.tileSpacing * positionX;
        float myY = this.tileSize * this.tileSpacing * positionY;

        
        float p6nOffX = myX - halfWidth + (this.tileSize / 2.0f);
        float p6nOffY = myY - halfHeight + (this.tileSize / 2.0f);
        GameObject obj = Instantiate (tilePrefab, 
                                      new Vector3 (p6nOffX, p6nOffY), 
                                      Quaternion.identity) as GameObject;
        obj.renderer.material.mainTextureOffset = new Vector2(0f, 0.66f);
        return obj;
    }
}
